#Author:Arko
#Creation date: 05 April,2017
#Last Modifed: 08 April,2017

#install packages if not installed
packages.to.install <- c("stockPortfolio","quantmod","quadprog","ggplot2","corrplot","TTR","dplyr","broom","magrittr","plotly")
install.packages(packages.to.install)
#load the requisite packages
lapply(packages.to.install,require,character.only = TRUE)
#suppress unnecessary warning message
options("getSymbols.warning4.0"=FALSE)
#fetch list of stocks and associated tickers
stockdetails <- stockSymbols()
#sort by Symbol
p = order(xtfrm(stockdetails$Symbol))
stockdetails <- stockdetails[p,]
#select only the Symbol and Name of stocks
stockdetails <- select(stockdetails,c(Symbol,Name))
#it was observed that some stock names have duplicate tickers. We should select only one
stockdetails <- subset(stockdetails,!duplicated(stockdetails[,2]))
View(stockdetails)
#get number of stocks
number.stocks <- as.numeric(readline("Enter number of stocks you want in portfolio:"))
#accept the Name of the desired no. of stocks 
#define an empty vector to store the cooresponding tickers
stocks <- c()
for( i in 1:number.stocks){
  name <- readline("Enter name of the stock:")
  if(!(name %in% stockdetails$Name)){
    cat(name, "is not a valid stock name. Please enter proper stock name.")
    break();
  }
  else{
    ticker <- stockdetails[stockdetails$Name == name,]$Symbol
    stocks <- c(stocks,ticker)
  }
}
#Visualize  most recent performance(last one year) of the stocks in terms of their 
#monthly returns
data = lapply(stocks, function(sym) {
  monthlyReturn(na.omit(getSymbols(sym, from=Sys.Date()-365, auto.assign=FALSE)))
})
data <- do.call(merge,data)
names(data) <- stocks
index(data) <- as.Date(index(data))
tidy(data) %>% ggplot(aes(x=index,y=value, color=series)) + 
               geom_line() +
               ggtitle("Monthly Returns for Last 1 year") +
               theme(plot.title = element_text(face = "bold.italic",
                                  color="blue", 
                                  size=20, 
                                  hjust=0.5))+
               labs(x="Time Period", y = "Returns")
#Now with an overview of the recent performances we can now proceed towards 
#optimization process
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
#user input the desired frequency of returns(weekly,monthly,daily)
interval.frequency <- readline("Enter the frequency of returns(month,week,day):")
#get the returns for each of the stocks by the desired interval
stocks.returns <- getReturns(stocks, freq=interval.frequency,
                      start = start.date,end = end.date)
#get the mean return for each of the stocks
summary(stocks.returns)
#get the matrix of returns for the stocks
#the returns are calculated using the current and historical values of the stock with lag 1
returns.data <- stocks.returns$R
View(returns.data)

#function to implement efficient frontier algorithm. Following are description of parameters:
# data: the matrix of returns for the stocks
# max.alloc.pct: the maximum percentage allocation that can be allowed for any stock
# short.selling: enter yes if we want to allow short-selling is allowed
# max.risk: maximum risk allowed.Depends on Nature of Customer
# incr.value.risk : incremental value by which risk factor is to be increased during optimisation loop
efficient.frontier <- function (data, max.alloc.pct=NULL, short.selling="no", max.risk=0.3, incr.value.risk=.002){
  #calculate and plot the correlation between the different stocks
  corrplot(cor(data))
  #calculate covariance. This will be the matrix in the quadratic function to be
  #minimized
  covar <- cov(data)
  #print(covar)
  #get the number of stocks. This will be useful in future calculations and checking
  #for correct value of maximum allocation percentages
  n <- number.stocks
  #Initialize the matrix defining the constraints to be used for minimizing the quadratic function
  constraints.matrix <- matrix (1, nrow=n)
  #initialize the vector against whom the contraints matrix will be compared
  b_intercept <- 1
  # define the column in constraints matrix which is to be used as an equality constraint
  #for our analysis we are considering only one equality constraint that the sum of weights 
  #assigned to the different stock returns should be equal to 1
  equality.constraint.col <- 1
  #hence in the constraints matrix, the first column will be assigned as 1
  # Define the constraints matrix and b_intercept values if short-selling is not allowed
  #if short-selling is allowed, weights to be assigned to the stocks during optimisation
  #can be negative
  if(short.selling == "no"){
    constraints.matrix <- cbind(1, diag(n))
    b_intercept <- c(b_intercept, rep(0, n))
  }
  #If maximum allocation percentage is specified,
  #we need to first check if the allocation % is accepatble
  #If so, we have to recalculate the values of constraints matrix and b_intercept accordingly
  if(!is.null(max.alloc.pct)){
    #check if the percentage value is between 0 and 1
    if(max.alloc.pct > 1 | max.alloc.pct <0){
      #generate error message if the test fails
      stop("The maximum allocation percentage should be a value between 0 and 1")
    }
    #check if a sufficiently high value of maximum allocation percentage has been set
    #Else the objective of the sum of all weights to 1 will not be satisfied
    if(max.alloc.pct * n < 1){
      #generate error message if the test fails
      stop("A higher value of maximum allocation percentage is required")
    }
    #after running the checks, modify the constraints matrix and the b_intercept
    constraints.matrix <- cbind(constraints.matrix, -diag(n))
    b_intercept <- c(b_intercept, rep(-max.alloc.pct, n))
  }
  #Calculate the number of times the quadratio optimizer will run using
  #the maximum allowable risk and the risk incremental value
  times.optimize <- (max.risk / incr.value.risk) + 1
  counter <- 1
  #Initialize the matrix to store the calcuated weights during optimisation as well
  #as the corresponding expected return, volatility and sharpe ratio 
  efficient.frontier.matrix <- matrix(nrow=times.optimize, ncol=n+3)
  colnames(efficient.frontier.matrix) <- c(colnames(data), "Volatility", "ExpectedReturn", "SharpeRatio")
  #Execute the loop for quadratic optimisation for implementing the efficient frontier algorithm
  for (i in seq(from=0, to=max.risk, by=incr.value.risk)){
    #calculate the vector in the quadratic function to be minimized
    #this is the average returns for each stock
    vector.minimize <- colMeans(data) * i 
    #execute the quadratic optimizer
    quad.opt <- solve.QP(covar, 
                         dvec=vector.minimize, 
                         Amat=constraints.matrix, 
                         bvec=b_intercept, 
                         meq=equality.constraint.col)
    #the optimized weights will be in the solution parameter of the optimizer object
    efficient.frontier.matrix[counter,1:n] <- quad.opt$solution
    efficient.frontier.matrix[counter,"Volatility"] <- sqrt(sum(quad.opt$solution *colSums((covar * quad.opt$solution))))
    efficient.frontier.matrix[counter,"ExpectedReturn"] <- as.numeric(quad.opt$solution %*% colMeans(data))
    efficient.frontier.matrix[counter,"SharpeRatio"] <- efficient.frontier.matrix[counter,"ExpectedReturn"] / efficient.frontier.matrix[counter,"Volatility"]
    #increment counter for next row of optimized values
    counter = counter+1
  }
  #return the final dataframe containing the optimized weights and corresponding assessment metrics
  return(as.data.frame(efficient.frontier.matrix))
}
#Invoke the efiicient frontier function
optimized.values <- efficient.frontier(data=returns.data, max.alloc.pct=.5 ,short.selling="yes", max.risk=.2, incr.value.risk=.001)
head(optimized.values)
#The desired allocation will be the weights giving maximum Sharpe ratio
optimal.allocation <- optimized.values[optimized.values$SharpeRatio==max(optimized.values$SharpeRatio),]
#plot the optimisation values and highlight the one with highest Sharpe ratio
#For ggplot2 color refer:
#http://sape.inf.usi.ch/quick-reference/ggplot2/colour
p <- ggplot(optimized.values, aes(x=Volatility, y=ExpectedReturn,label = SharpeRatio))+
  geom_point(alpha=.1, color= "coral")+
  geom_point(data=optimal.allocation, color="coral", size=6)+
  ggtitle("Portfolio Optimisation using Efficient Frontier Algorithm")+
  labs(x="Risk", y="Expected Return")+
  theme(panel.background=element_rect(fill="ivory3"), text=element_text(color="black"),
        plot.title=element_text(size=24, color="blue"))
ggplotly(p)



