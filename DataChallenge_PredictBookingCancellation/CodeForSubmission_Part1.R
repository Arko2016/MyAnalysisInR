#set path
path <- "C:/Users/Arko/Downloads/PrincessCruisessDataChallenge/"
setwd(path)
#check if relevant files are present in the directory
list.files()
#load libraries (install if not done before)
library(dplyr)
library(xgboost)
library(magrittr)
library(XLConnect)
library(dummies)
library(caret)
library(corrplot)

#load the data
wb = loadWorkbook("R Assessment_Retention Dataset.xlsx")
data = readWorksheet(wb, sheet = "Revenue Science Interview Reten", header = TRUE)

#Exploratory data analysis and feature engineering
#check summary statistics
summary(data)
#get class of each attribute
sapply(data,class)
#get % missing values for each attribute
sapply(data,function(x){(sum(is.na(x))/length(x))*100})
#For the target variable, BKNG_STATUS, there are 2 observations denoted by "G"
#However, the data dictionary states there are only 2 levels for BKNG_STATUS : B, C
#if we inspect the BKNG_CANCEL_DATE for the BKNG_STATUS, we observe that these two 
#passengers did not cancel their ticket. hence we will club them with non-cancelled passengers
data[data$BKNG_STATUS == "G",]$BKNG_STATUS <- "B" 
#For gender, we replace the missing values by "Unknown" 
data$GENDER[is.na(data$GENDER)] <- "Unknown" 
#Age only has 1%(32 values missing). Let us check out the median age for People who booked
#and people who cancelled
data %>% group_by(BKNG_STATUS) %>% summarise_at(vars(AGE),funs(median(.,na.rm=T)))
#clearly there is a difference in age between the median ages of the two groups.
#Hence it makes sense to impute missing values with median according to the BKNG_STATUS
#also there are 22 values where AGE is 0, which is not correct. hence replacing them
#with median as well. Median more robust to outliers
data[(is.na(data$AGE) | data$AGE == 0) & data['BKNG_STATUS'] == "B",]$AGE <- median((data[data['BKNG_STATUS'] == "B",]$AGE),na.rm=T)
data[(is.na(data$AGE) | data$AGE == 0) & data['BKNG_STATUS'] == "C",]$AGE <- median((data[data['BKNG_STATUS'] == "C",]$AGE),na.rm=T)
#for the remaining 4 columns with missing values , we will replace them will value "Unknown"
#this is because it might be erraneous to impute them based on other attributes 
#as they are categorical and can have any probable value out of the non-missing values
data[is.na(data)] <- "Unknown"
#we observe that 2008 of the citizens have US Nationality. hence creating
#a new feature indicating US citizen or not. 1= US citizen
data %<>% mutate(US_Citizen = ifelse(NATIONALITY == "US",1,0))
#Again for INSURANCE_PYMT_DATE, we observe that most of the passengers have not availed
#insurance, hence creating a new feature to indicate insurance purchased or not.
#1 = not purchased
data %<>% mutate(Insurance_purchased = ifelse(as.character(INSURANCE_PYMT_DATE) == as.Date("1900-12-31"),1,0))
#For NBR_CRUISES, there are 13 records which are > greater than 10
#some of these are prominent outliers as well. Hence capping all values > 10 as 11
boxplot(data$NBR_CRUISES)
data['NBR_CRUISES'] <- ifelse(data$NBR_CRUISES > 10, 11,data$NBR_CRUISES)
boxplot(data$NBR_CRUISES)
#we create a new feature Resident_or_Immigrant indicating that
# if nationality = home_country, then resident(1) else immigrant(0)
#the intuition is that people may have passport issues for which they had to cancel
#the booking
data['Resident_or_Immigrant'] = ifelse(data$NATIONALITY == data$HOME_COUNTRY,1,0)
#2130 of the booking payments were made in USD. hence creating a new feature
#indicating USD payment or not. 1 = USD payment
data %<>% mutate(USD_Payment = ifelse(data$CURRENCY_CODE == "USD",1,0))
#create dummy variables for some of the categorical variables which do not have
#large number of levels
data <- dummy.data.frame(data,names = c("AIR_FLAG","BKNG_SOURCE",
                                      "BKNG_TYPE","GENDER","DINING_TIME_CODE",
                                       "META","GROUP_TYPE"))
#drop unnecessary columns
data <- subset(data,,-c(HOME_STATE,NATIONALITY,HOME_COUNTRY,BKNG_OPEN_DATE,BKNG_CANCEL_DATE,
                       CURRENCY_CODE,DINING_TIME_CONF_FLAG,INSURANCE_PYMT_DATE,SAIL_DATE))
write.csv(data,"processeddata.csv")
#convert the target variable to binary. 0= active booking, 1 = cancelled
data$BKNG_STATUS <- as.factor(ifelse(data$BKNG_STATUS == "B",0,1))
#save the columns to be used for predictors
predictor.cols <- names(subset(data,,-c(BKNG_ID,BKNG_STATUS)))
data.target <- data$BKNG_STATUS
data.id <- data$BKNG_ID
data$BKNG_STATUS <- NULL
data$BKNG_ID <- NULL
#normalize these columns
norm.func <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
data[,sapply(data,is.numeric)] <- as.data.frame(lapply(data[,sapply(data,is.numeric)],norm.func))
#join back the target and id columns
data$BKNG_STATUS <- data.target
data$BKNG_ID <- data.id

#now that we have completed pre-processing and feature engineering,
#randomly divide the data into train and test in the ratio 75%-25% respectively
sample.size <- floor(0.75 * nrow(data)) 
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = sample.size)
train.data <- data[train_ind, ]
test.data <- data[-train_ind, ]
#checking for zero variance based on PCA.The intuition is that attributes should
#have maximum variance, so that each attribute is as distant as possible from the other 
zero.var = nearZeroVar(train.data, saveMetrics=TRUE)
View(zero.var)
#We observe that few of the dummy variables created have low variability.
#one more observation is that USD_payment attribute has low variability. This aligns with
#our observation that most of the passengers have made payment in USD so the column 
#lacks variability
#we will keep these points in mind while selecting the set of features for designing
#the final model

#although we considered various models, we found that randomforest and xgboost
#classification proved to be performing better than the rest in terms of robust to 
#outliers, accuracy, computation, etc.
#hence we will go ahead and try out the same

#Model1: randomforest
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:sqrt(ncol(train.data))))
rf_gridsearch <- train(BKNG_STATUS~., data=subset(train.data,,-c(BKNG_ID)), 
                       method="rf", metric="Accuracy", 
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#store the best model
print(rf_gridsearch$finalModel)
bst.rf <- rf_gridsearch$finalModel
#get the variable importances
varImp(bst.rf)
varImpPlot(bst.rf)
preds.rf <- predict(bst.rf,subset(test.data,,-c(BKNG_STATUS,BKNG_ID)))
confusionMatrix (preds.rf, test.data$BKNG_STATUS)

#Model2: xgboost
#develop xgboost model for classification
# convert data to matrix
train.matrix = as.matrix(train.data[,predictor.cols])
mode(train.matrix) = "numeric"
test.matrix = as.matrix(test.data[,predictor.cols])
mode(test.matrix) = "numeric"
train.target = as.matrix(train.data$BKNG_STATUS)
numbr.classes = length(unique(data$BKNG_STATUS))
#set xgboost parameters
# xgboost parameters
parameters <- list("objective" = "binary:logistic",
              "booster" = "gbtree",
              "eval_metric" = "logloss",     
              "max_depth" = 10,    
              "eta" = 0.3,    
              "gamma" = 2,    
              "subsample" = 1,    
              "colsample_bytree" = 1,   
              "min_child_weight" = 12   
)
#tuning parameters for xgboost
set.seed(1234)
#perform 5-fold cross validation for 200 iterations
nround.cv = 100
nfolds.cv = 5
bst.cv <- xgb.cv(param=parameters, data=train.matrix, label=train.target, 
                 nfold=nfolds.cv, nrounds=nround.cv, prediction=TRUE, verbose=FALSE)
tail(bst.cv$dt)
#get the index of minimum logloss error
min.logloss.idx = which.min(bst.cv$dt[, test.logloss.mean]) 
min.logloss.idx
# get the values of minimum logloss error
bst.cv$dt[min.logloss.idx,]

model.xgb <- xgboost(param=parameters, data=train.matrix, label=train.target, 
                     nrounds=min.logloss.idx, verbose=0)
preds.xgb <- predict(model.xgb,test.matrix)
preds.xgb <- ifelse (preds.xgb > 0.7,1,0)
confusionMatrix (preds.xgb, test.data$BKNG_STATUS)

#Based on model accuracy metrics and complexity we will decide to go with Randomforest model
#let us try to improve the model further
#based on the variances of the predictors and variable importance
#we will try to filter out some predictors and see if it improves model significantly
train.data <- subset(train.data,,-c(GROUP_TYPEF,GROUP_TYPEK,GROUP_TYPEQ,
                                    AIR_FLAGL,AIR_FLAGUnknown,GENDERUnknown,
                                    USD_Payment))
test.data <- subset(test.data,,-c(GROUP_TYPEF,GROUP_TYPEK,GROUP_TYPEQ,
                                    AIR_FLAGL,AIR_FLAGUnknown,GENDERUnknown,
                                    USD_Payment))
#Now we re-run the rf model
#Model3: final randomforest
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(123)
tunegrid <- expand.grid(.mtry=c(1:sqrt(ncol(train.data))))
rf_gridsearch <- train(BKNG_STATUS~., data=subset(train.data,,-c(BKNG_ID)), 
                       method="rf", metric="Accuracy", 
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
#store the best model
print(rf_gridsearch$finalModel)
bst.rf <- rf_gridsearch$finalModel
#get the variable importances
varImp(bst.rf)
varImpPlot(bst.rf)
preds.rf <- predict(bst.rf,subset(test.data,,-c(BKNG_STATUS,BKNG_ID)))
confusionMatrix (preds.rf, test.data$BKNG_STATUS)
#the OOb error rate decreased slightly. So we will go ahead and consider this as our 
#final model
