# Reference links:
#http://www.listendata.com/2016/08/dplyr-tutorial.html

rm(list = ls())
library(dplyr)
data <- read.csv("C:\\Users\\Arko\\Downloads\\Studies\\PersonalResearch\\DplyrData.csv",
                 header = TRUE)
dim(data)
View(data)
#Example 1 : Selecting Random N Rows
samplen <- sample_n(data,20) 
#Example 2 : Selecting Random Fraction of Rows
#we specify the % of rows to be selected
samplefrac <- sample_frac(data,0.2)
#Example 3 : Remove Duplicate Rows based on all the variables (Complete Row)
data.distinct <- distinct(data)
#Example 4 : Remove Duplicate Rows based on a variable
#.keep_all parameter helps to retain all variables
data.distinct <- distinct(data,Index,.keep_all = TRUE)
#Example 5 : Remove Duplicates Rows based on multiple variables
data.distinct <- distinct(data,Index,Y2010,.keep_all = TRUE)
#Example 6 : Selecting Variables (or Columns)
data.subset <- select(data,Index,State,Y2002)
#Example 7 : Dropping Variables
#the '-' sign takes care of dropping variables
data.subset <- select(data,-Y2002,-Y2008)
#Example 8 : Selecting or Dropping Variables starts with 'Y'
data.subset <- select(data,starts_with("Y"))
#the '-' sign before starts_with() drops those variables starting with Y
data.subset <- select(data,-starts_with("Y"))
#Example 9 : Selecting Variables contain 'I' in their names
data.subset <- select(data,contains("I"))
#Example 10 : Reorder Variables to keep State in front and rest of the variables later
data.reorder <- select(data,State,everything())
#Example 11 : Rename Variables . Eg. Index as Index1
data.rename <- rename(data,Index1 = Index)
#Example 12 : Filter Rows and retain only those values in which Index is equal to A
data.filter <- filter(data,Index == 'A')
#Example 13 : Multiple Selection Criteria to select rows against 'A' and 'C' in column 'Index'
data.filter <- filter(data,Index %in% c("A","C"))
#Example 14 : 'AND' or 'OR' Condition in Selection Criteria
data.filter <- filter(data,Index %in% c("A","C") & Y2002 >= 1300000)
data.filter <- filter(data,Index %in% c("A","C") | Y2002 >= 1300000)
#Example 16 : NOT Condition
data.filter <- filter(data,!Index %in% c("A","C"))
#Example 17 : grepl function is used to search for pattern matching. 
#In the following code, we are looking for records wherein column state 
#contains 'Ar' in their name
data.filter <- filter(data,grepl("Ar",State))
#Example 18 : calculating mean and median for the variable Y2015 and store them
#in new variables
#Note: Here we are summarising only a single variable
summarise(data,Y2015_mean = mean(Y2015),Y2015_median = median(Y2015))
#Example 19:calculating number of records, mean and median for variables Y2005 and Y2006
#Note: here we are calculating multiple summary functions for multiple variables
#The output will be in the form of a dataframe
summarise_at(data,vars(Y2005,Y2006),funs(n(),mean,median))
#Example 20 : Summarize with Custom Functions
#use custom functions in the summarise function. In this case, we are computing the 
#number of records, number of missing values, mean and median for variables 
#Y2011 and Y2012. The dot (.) denotes each variables specified in the second 
#argument of the function
summarise_at(data,vars(Y2011,Y2012),funs(n(),
                                         missing = sum(is.na(.)),
                                         mean(.,na.rm=TRUE),
                                         median(.,na.rm = TRUE)))
#Example 21: apply Non-Standard Functions
#subtract mean from its original value and then calculate variance of it
set.seed(222)
mydata <- data.frame(X1=sample(1:100,100), X2=runif(100))
summarise_at(mydata,vars(X1,X2),function(x){var(x - mean(x))})
#Example 22 : Summarize all Numeric Variables
#this can be done in 2 ways
#1st method
summarise_if(data,is.numeric,funs(n(),mean,median))
#2nd method
data.subset <- data[sapply(data,is.numeric)]
#summarise_all will calculate the functions for all the variables
#Note: if summarise_at is used, then vars() parameter needs to be passed
#which will specify the variables
summarise_all(data.subset,funs(n(),mean,median))
#in short,
summarise_all(data[sapply(data,is.numeric)],funs(n(),mean,median))
#Example 22 : Summarize Factor Variable
#We are checking the number of levels/categories and count of missing observations 
#in a categorical (factor) variable
summarise_all(data['Index'],funs(nlevels(.),sum(is.na(.))))
#Example 23 : Sort Data by Multiple Variables
data.sorted <- arrange(data,desc(Index),Y2011)
library(magrittr)
#Example 24:selecting 10 random observations of two variables "Index" 
#"State" from the data
data.subset <- data %>% select(Index,State) %>% sample_n(10)
#Example 25:calculating count and mean of variables Y2011 and Y2012 by variable Index
data.subset <- data %>% group_by(Index) %>%
                        summarize_at(vars(Y2011,Y2012),funs(Count = n(),mean(.,na.rm=TRUE)))
              
#Note : do() is used to compute operations WITHIN the LEVELS of a CATEGORICAL variable
#Example 25 : Filter Data within a Categorical Variable
#pull top 2 rows from 'A', 'C' and 'I' categories of variable Index
data.subset <- data %>% filter(Index %in% c('A','C','I')) %>%
                        group_by(Index) %>%
                        do(head(.,2))
#calculating third maximum value of variable Y2015 by variable Index.
#The following code first selects only two variables Index and Y2015. 
#Then it filters the variable Index with 'A', 'C' and 'I' and then it groups 
#the same variable and sorts the variable Y2015 in descending order. At last, 
#it selects the third row
data.subset <- data %>%
               select(Index,Y2015,Y2008) %>%
               filter(Index %in% c("A","C","I")) %>%
               group_by(Index) %>%
               do(arrange(.,desc(Y2015))) %>%
               slice(3)
#Note: The slice() function is used to select rows by position.
#Example 27 :computing mean of variables Y2014 and Y2015 by variable Index.
#Then sort the result by calculated mean variable Y2015
data.subset <- data %>%
               select(Index,Y2014,Y2015) %>%
               group_by(Index)%>%
               summarise(Mean_Y2014 = mean(Y2014,na.rm=T),
                         Mean_Y2015 = mean(Y2015,na.rm=T))%>%
               arrange(desc(Mean_Y2015))
#Example 28 : Create a new variable
#calculates division of Y2015 by Y2014 and name it "change"
data.new = mutate(data,change = Y2015/Y2014)
#Example 29 : Multiply all the Numeric variables by 1000
#this will create new variables with _new extension
data.subset <- data[sapply(data,is.numeric)] %>%
               mutate_all(funs(new=.*1000))
#This will make changes inplace
data.subset1 <- data[sapply(data,is.numeric)] %>%
       mutate_all(funs(.*1000))
#Example 30 : calculate rank for variables Y2008 to Y2010
data.subset <- mutate_at(data,vars(Y2008:Y2010),funs(Rank = min_rank(.)))
#By default, min_rank() assigns 1 to the smallest value and high number to the largest
#value. In case, you need to assign rank 1 to the largest value of a variable, 
#use min_rank(desc(.))  
data.subset <- mutate_at(data,vars(Y2008:Y2010),funs(Rank = min_rank(desc(.))))
#Example 31 : Select State that generated highest income in 2015 among the variable 'Index'
#here no need to apply do() since among all states operation is required
data.subset <- data %>%
               select(Index,Y2015,State)%>%
               group_by(Index)%>%
               arrange(.,desc(Y2015))%>%
               slice(1)
#if we wanted 2nd highest. do slice(2)
#Example 32 : Cumulative Income 2015 of 'Index' variable
#cumulative sum is calculated using function 'cumsum'
data.subset <- data %>% 
               select(Index,Y2015)%>%
               group_by(Index)%>%
               mutate(Total = cumsum(Y2015))
#Example 33 : Common rows in both the tables
#create 2 dataframes
df1 <- data.frame(ID = c(1, 2, 3, 4, 5),
                  w = c('a', 'b', 'c', 'd', 'e'),
                  x = c(1, 1, 0, 0, 1),
                  y=rnorm(5),
                  z=letters[1:5])
df2 <- data.frame(ID = c(1, 7, 3, 6, 8),
                  a = c('z', 'b', 'k', 'd', 'l'),
                  b = c(1, 2, 3, 0, 4),
                  c =rnorm(5),
                  d =letters[2:6])
#inner join
df3 = inner_join(df1,df2,by = 'ID')
#left join
df4 = left_join(df1,df2,by = 'ID')
#semi_join(): Include rows of x that match y but only keep the columns from x
df5 <- semi_join(df1,df2,by = 'ID')
#anti_join: opposite of semi_join
df6 <- anti_join(df1,df2,by = 'ID')
#Hence anti_join returns those rows from df1 which are not included by df5
#diiference between intersect, union and setdiff
# intersect(x, y)
# Rows that appear in both x and y.
# 
# union(x, y)
# Rows that appear in either or both x and y.
# 
# setdiff(x, y)
# Rows that appear in x but not y.
#prepare sample data
first <- mtcars[1:20,]
second <- mtcars[10:20,]
View(intersect(first,second))
#union -> shows the rows in both the dataframes Removing duplicates
View(union(first,second))
#union_all -> shows the rows in both the dataframes Without Removing duplicates
View(union_all(first,second))
View(setdiff(first,second))
#Example 39 :  find maximum value in each row of variables 2012, 2013, 2014, 2015 using
#rowwise
data.subset <- data %>% 
               rowwise()%>%
               mutate(Max = max(Y2010:Y2015))%>%
               select(Y2010:Y2015,Max)
#Example 41 : Calculate Percentile Values
data %>% summarise(Percentile_25 = quantile(Y2015,probs = 0.25),
                            Percentile_50 = quantile(Y2015,probs = 0.5),
                            Percentile_75 = quantile(Y2015,probs = 0.75),
                            Percentile_99 = quantile(Y2015,probs = 0.99))
#ntile() function is used to divide the data into N bins
x= data.frame(N= 1:25,vals = runif(25,1,50))
x = mutate(x, pos = ntile(x$N,5))
#select only factor columns
data.subset <- data %>% select_if(is.factor)
#Example 42 : Number of levels in factor variables
data.subset <- data %>% summarise_if(is.factor,funs(nlevels(.)))
#Example 43 : Convert value to NA
k <- c("a", "b", "", "d")
na_if(k, "")






