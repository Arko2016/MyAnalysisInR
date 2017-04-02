#Author:Arko
#Creation date: 01 April,2017
#Last Modifed: 02 April,2017

#install packages if not installed
packages.to.install <- c("ggplot2","quantmod","xts","lubridate","forcats","rvest","tidyverse","stringr","corrplot","plotly")
install.packages(packages.to.install)
#load the requisite packages
lapply(packages.to.install,require,character.only = TRUE)

#quantmod package can be used to retrieve and manupulate stock prices
ticker <- readline("Enter the ticker for the desired stock:")
#enter start date of analysis
#check and validate if the start date was entered in the specific format
#if correct format is entered, flag will be 1 and exit while loop
flag = 0
while(flag == 0){
  start.date <- readline("Enter start date of analysis(yyyy-mm-dd):")
  start.date <- try(as.Date(start.date,format = "%Y-%m-%d"))
  if(class(start.date) == "try-error" || is.na(start.date)){
    print("Please enter date in the format yyyy-mm-dd")
  }
  else{
    flag = 1
  }
}
#repeat the same validation while entering end date of analysis
flag = 0
while(flag == 0){
  end.date <- readline("Enter end date of analysis(yyyy-mm-dd):")
  end.date <- try(as.Date(end.date,format = "%Y-%m-%d"))
  if(class(end.date) == "try-error" || is.na(end.date)){
    print("Please enter date in the format yyyy-mm-dd")
  }
  else{
    flag = 1
  }
}
#retrieve stock information using getsymbols function
#suppress unnecessary warning message
options("getSymbols.warning4.0"=FALSE)
#auto.assign parameter returns the stock information to a generic variable
stock.data <- getSymbols(Symbols = ticker,from = start.date,to = end.date,auto.assign = FALSE)
class(stock.data)
#View the structure of the stock data retrieved
str(stock.data)
head(stock.data)
#The data consists of prices and volumes of the stock traded on daily basis
#Visulaize the Prices of the stock for a given year, say 2015
year.underconsideration <- readline("Enter the year for which we want to visualize the stock
                                    performance:")
#Following are few parameters to be taken under consideration while viewing the chart
#Bollinger bands : indicator of volatility. Sudden change in volatility indicates market trend reversal
#                   The Bollinger line is bounded by the pricing intervals
#The draw=p parameter: indicates the Bollinger banda in percentage value
#addVo : gives the stock volume
#addMACD : gives the Moving average convergence divergence.
#          If the MACD falls below the Signal/Trigger line -> bearish phase -> time to sell
#          If the MACD falls above the Signal/Trigger line -> bullish phase -> time to buy
stock.data %>%
  chartSeries(TA='addVo();
              addBBands();
              addBBands(draw="p");
              addMACD()', 
              subset=year.underconsideration,
              theme="black"
)
#the subset can also be done with a from and to parameter. But for simplifying our
#analysis, we have limited it to a yearly level
#to nullify the effect of stock splits, we will use the Adjusted prices for our calculations
#The dailyreturn function will give the Adjusted log returns for the stock
#log value of the returns is considered so that we can have a normal distribution which
#makes the analysis more manageable
stock.data.dailyreturns <- stock.data %>% Ad() %>% 
                           dailyReturn(type="log")
head(stock.data.dailyreturns)
names(stock.data.dailyreturns) <- "stock.data.logreturns"
#visualise the dailyreturns
stock.data.dailyreturns %>%    
  ggplot(aes(x = stock.data.logreturns)) + 
  geom_histogram(bins = 200) + 
  geom_density()+
  ggtitle("Daily log returns")+
  theme(plot.title = element_text(face = "bold.italic",
                                  color="blue", 
                                  size=20, 
                                  hjust=0.5))+
  labs(x="Log returns")
#Now we can obtain the mean and stadard deviation of the daily returns
mean.dailyreturn <- mean(stock.data.dailyreturns,na.rm=TRUE)
sd.dailyreturn <- sd(stock.data.dailyreturns,na.rm=TRUE)
#indicates that on an average, the mean daily return is approximately 0.095% more than the 
#previous day's stock price with a volatility of 0.016

#Using the above Mean daily return and standard deviation values for the stock,
#we simulate a random walk/process of around 252 samples since
#generally there are 252 trading days in a year. We run a Monte-Carlo simulation process
#This comprises of running the random process for around 300 times and helps us to get an understanding
#about the different possible scenarios including the most likely best and worst cases
number.randomwalk.simul <- 252
number.montecarlo.simul <- as.numeric(readline("Enter Number of Monte Carlo Simulations
                                    (preferably > no. of random walk simulations)"))
#We will create a matrix where in we will run the entered number of Monte Carlo simulations
#for each of the 252 trading days using the mean and std. of daily returns calculated above
trading.days <- 1:number.randomwalk.simul
#set suitable names for the stock data 
names(stock.data) <- c("Open","High","Low","Close","Volume","AdjPrice")
#set the initial price as the the final adjusted price of the stock
init.price <- stock.data$AdjPrice[[nrow(stock.data$AdjPrice)]]
#run the MonteCarlo simulation for creating the above mentioned matrix of stock price simulations
#since this is a random process, we initialize a seed value
set.seed(1)
#define an empty matrix
mc_matrix <- matrix(nrow = number.randomwalk.simul, ncol = number.montecarlo.simul)
for (j in 1:number.montecarlo.simul) {
  #the first value in the row will be the initial adjusted stock price
  mc_matrix[[1, j]] <- init.price
  for(i in 2:number.randomwalk.simul) {
    #the subsequent values are set by exponential smoothing formula
    #exponential values are taken to get the original values of mean and standard deviations
    #from the log normal forms
    mc_matrix[[i, j]] <- mc_matrix[[i - 1, j]] * exp(rnorm(1, mean.dailyreturn,sd.dailyreturn ))
  }
}
#We bind the values into a tibble
#a tibble is a better way to represent the dataframe
simulated.prices <- as_tibble(cbind(trading.days, mc_matrix))
#generate dynamic names to be set as column names for the tibble
#for each simulation, the number of columns will be equal to the number of Montecarlo simulations
names.simulations <- str_c("Simulation_", seq(1, number.montecarlo.simul))
#rename the first column as TradingDay
names.simulations <- c("TradingDay", names.simulations)
#set the names of the tibble
names(simulated.prices) <- names.simulations
#the simulated prices tibble is re-shaped for ease of visualization
simulated.prices <- simulated.prices %>%
  gather(key = "SimulationTerm", value = "AdjustedStockPrice", -(TradingDay))
# We can now observe the simulated adjusted prices for each trading day 
#grouped by the simulation terms
simulated.prices %>%
  ggplot(aes(x = TradingDay, y = AdjustedStockPrice, Group = SimulationTerm)) + 
  #alpha is transparency parameter
  geom_line(alpha = 0.2,color = "steelblue") +
  ggtitle(paste("Monte-Carlo Simulated Adjusted Stock Prices for", number.randomwalk.simul, 
                " Trading Days"))+
  theme(plot.title = element_text(face = "bold.italic",
                                  color="blue", 
                                  size=20, 
                                  hjust=0.5))+
  labs(x="Trading Days",y="Simulated Stock price($)")
#we would now like to validate how realistic are our simulated prices
#Compound Annual growth Rate(CAGR) is the mean annual rate of an investment over a specified
#duration(usually for more than a year)
#Since the annual growth rate is inconsistent, CAGR gives an average growth rate to give
#a holistic picture of the stock's progress
#First we calculate the historical CAGR
number.historicalobsv <- nrow(stock.data)/252
startingvalue <- stock.data$AdjPrice[[1]]
endingvalue <- stock.data$AdjPrice[[nrow(stock.data)]]
cagr.historical <- (endingvalue / startingvalue) ^ (1 / number.historicalobsv) - 1
#Next we calculate the simulated CAGR
#We find the simulated prices on the last trading day and store the most likely(median) value
simul.lastdayprices <- simulated.prices %>% 
  filter(TradingDay == max(TradingDay))
#this will be the ending value while calculating simulated CAGR
median.simul.lastdayprices <- median(simul.lastdayprices$AdjustedStockPrice,na.rm=TRUE)
#the starting value for simulated CAGR should be the final value of the actual Adjusted prices
simul.starting.value <- endingvalue
number.simulobsv <- number.randomwalk.simul/252
cagr.simul <- (median.simul.lastdayprices / simul.starting.value) ^ (1 / number.simulobsv) - 1
#Since the CAGR values are close, we can consider that our simulations match with the realistic prices
