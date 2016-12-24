#modify memory size
options(java.parameters = "-Xmx10g" )

#install required packages if required
install.packages("devtools")
require("devtools")
install_github("ujjwalkarn/xda",force = TRUE) #needed for xda package
list.of.packages <- c("xlsx","data.table","dummies","caret","xgboost","gbm","dismo",
                      "e1071","scales","plyr","glmnet","foreach")
require("xda")
install.packages(list.of.packages)
#load required packages
lapply(list.of.packages,require,character.only=TRUE)

#set path
setwd("C:/Users/Arko/Downloads/Studies/PersonalResearch/Travelers Case Competition/")
#load the file containing GiniIndex Calculation function
source("GiniIndexCalculation.R")

#read the train and test files 
original.traindata <- read.csv("Data/Kangaroo_train.csv" , header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
original.validationdata <- read.csv("Data/Kangaroo_valid.csv" , header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
original.testdata <- read.csv("Data/Kangaroo_hold.csv" , header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))

#base model
gbm_fit=gbm.step(original.traindata,gbm.x=3:8,
                 gbm.y=2,
                 family="gaussian",
                 n.trees= 50,
                 tree.complexity=2,
                 bag.fraction=0.2,
                 learning.rate=0.1)
#predicting on validation data
valid_gbm<- predict.gbm(gbm_fit,original.validationdata,n.trees=gbm_fit$gbm.call$best.trees,type="response")
write.csv(valid_gbm,"BaselineModel_preds.csv")
#calculating Gini index from function
data.for.giniFunction <- as.data.frame(cbind(original.validationdata$claimcst0,valid_gbm))
colnames(data.for.giniFunction) <- c("actual","predicted")
get.GINI(data.for.giniFunction,py = "predicted", y = "actual")

#combine test,validation and train for data preprocessing into eda.data
traindata <- subset(original.traindata,select = -id)
cvdata <- subset(original.validationdata,select = -id)
testdata <- subset(original.testdata,select = -id)
traindata$ID <- "train"
cvdata$ID <- "validation"
testdata$ID <- "test"
testdata$claimcst0 <- 0 #adding this column for combining. Will be removed later
eda.data <- rbind(traindata,cvdata,testdata)
#removing columns clm and numclaim as per modified problem statement
eda.data <- eda.data[,!names(eda.data) %in% c('clm','numclaims')]

#observing summary statistics for the variables belonging to different data types
sapply(eda.data,class) #class of each variable as deemed by R
numSummary(eda.data)
charSummary(eda.data)
#we observe that data does not have any missing values
#checking outliers
boxplot(eda.data$veh_value)
boxplot(eda.data$agecat)
boxplot(eda.data$claimcst0)
#veh_value and claimcst0 have large number of outliers
#treating for outliers for veh_value by capping
qnt <- quantile(eda.data$veh_value,probs=c(0.25,0.75),na.rm=T)
caps <- quantile(eda.data$veh_value,probs=c(0.5,0.95),na.rm=T)
h <- 1.5*IQR(eda.data$veh_value,na.rm=T)
eda.data$veh_value <- ifelse(eda.data$veh_value < (qnt[1]-h),caps[1],eda.data$veh_value)
eda.data$veh_value <- ifelse(eda.data$veh_value > (qnt[2]+h),caps[2],eda.data$veh_value)
#check for outliers for veh_value after capping
boxplot(eda.data$veh_value)
#scaling veh_value after capping
eda.data$veh_value <- rescale(eda.data$veh_value,to=c(0,1))
#since claimcst0 has high number of outliers. log10 transformation was taken for the same
#for claimcst0 == 0, the value was remained unchanged since log10(0) is undefined
eda.data$claimcst0 <- ifelse(eda.data$claimcst0 == 0,1,eda.data$claimcst0)
eda.data$claimcst0 <- log10(eda.data$claimcst0)
#since veh_age has large number of categories, binning the categories to 2 categories
eda.data$veh_age <- ifelse(eda.data$veh_age %in% c(1,2),"v1-2",eda.data$veh_age)
eda.data$veh_age <- ifelse(eda.data$veh_age %in% c(3,4),"v3-4",eda.data$veh_age)
#similar approach not followed for age_cat since the gini-index turned out lower

#converting the veh_age,age_cat variables to dummies
eda.data <- dummy.data.frame(eda.data,names=c("veh_age","agecat"))
#converting gender to 0s and 1s. Male = 0, Female = 1
eda.data$gender <- ifelse(eda.data$gender == "M",0,1)

#keeping back-up
backup.data <- copy(eda.data)

#separating train,test and validate after data processing
traindata <- subset(eda.data,ID == "train")
cvdata <- subset(eda.data,ID == "validation")
testdata <- subset(eda.data,ID == "test")
#remove the ID column as they are no longer required now
traindata$ID <- NULL
cvdata$ID <- NULL
testdata$ID <- NULL
#remove target variable from testdata
testdata$claimcst0 <- NULL

#impact coding for variables veh_body,area(replacing the levels by corresponding frequencies)
variables.for.impactcoding <- c("veh_body","area")
func.impact.coding <- function(data,variables){
  sub.data <- data[,(names(data) %in% variables)]
  sub.data.transfrmd <- as.data.frame(apply(sub.data,2,function(x){
    t = as.data.frame(table(x))
    t$Freq[match(x,t[,1])]/length(x)
  }))
  data <- data[,!(names(data) %in% variables)]
  data <- cbind(data,sub.data.transfrmd)
  return(data)
}
traindata <- func.impact.coding(traindata,variables.for.impactcoding)
cvdata <- func.impact.coding(cvdata,variables.for.impactcoding)
testdata <- func.impact.coding(testdata,variables.for.impactcoding)

#Set tuning parameters
folds=5
repeats=2
control_params <- trainControl(method='repeatedCV', number=folds, repeats=repeats, returnResamp='none', 
                          returnData=FALSE, savePredictions=TRUE, 
                          verboseIter=TRUE, allowParallel=TRUE,
                          index=createMultiFolds(traindata$claimcst0, k=folds, times=repeats))
PP <- c('center', 'scale')

#Train glmnet model from caret package
targetcol <- "claimcst0"
predictors <- traindata[,!(names(traindata) %in% targetcol)]
model.glmnet <- train(predictors,traindata[,targetcol], method='glmnet', 
                      trControl=control_params,
                      tuneGrid = expand.grid(.alpha=seq(0,1,by=0.005),.lambda=c((1:5)/10)),
                      preProcess=PP)
cv.preds <- predict(model.glmnet,newdata = cvdata[,!(names(traindata) %in% targetcol)])
write.csv(cv.preds,"Glmnet_preds.csv")

#calculate giniindex for the model glmnet
data.for.giniFunction <- as.data.frame(cbind(original.validationdata$claimcst0,cv.preds))
colnames(data.for.giniFunction) <- c("actual","predicted")
gini_index <- get.GINI(data.for.giniFunction, py = "predicted", y = "actual")

#build xgboost model
library(xgboost)
param <- list(booster = "gblinear",
              #booster = "gbtree" , #do this for variable importance
              eta = 0.75,
              silent = 0,
              objective = "reg:linear",
              sample_type = "uniform",
              subsample = 0.75,
              lambda = 0.5,
              alpha = 0.01,
              gamma = 0,
              eval_metric = "rmse",
              max_depth = 10,
              nround = 5,
              tree_method = 'exact',
              nthread = 4
)

xgb.predictors = as.matrix(predictors)
xgb.target = as.matrix(traindata[,targetcol])
set.seed(1234)
nround.cv = 6
bst.cv <- xgb.cv(param=param, data=xgb.predictors, label=xgb.target, 
                 nfold=4, nrounds=nround.cv, prediction=TRUE, verbose=FALSE)
tail(bst.cv$dt)
# index of minimum merror
min.rmse.idx = which.min(bst.cv$dt[, test.rmse.mean]) 
min.rmse.idx
model.xgb <- xgboost(param=param, data=xgb.predictors, label=xgb.target, 
                     nrounds=min.rmse.idx, verbose=0)
xgb.cvdata <- as.matrix(cvdata[,!(names(traindata) %in% targetcol)])
preds.xgb <- predict(model.xgb,xgb.cvdata)
write.csv(preds.xgb,"xgboost_preds.csv")
#get variable importance
variable.significance <- as.data.table(xgb.importance(names(predictors), model = model.xgb))
write.csv(variable.significance,"variable_significance.csv")
install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.plot.importance(variable.significance)

#calculate giniindex  the model xgboost
data.for.giniFunction <- as.data.frame(cbind(original.validationdata$claimcst0,preds.xgb))
colnames(data.for.giniFunction) <- c("actual","predicted")
gini_index <- get.GINI(data.for.giniFunction, py = "predicted", y = "actual")


#predict for testdata using xgboost model since it has higher gini index on cvdata
xgb.testdata <- as.matrix(testdata)
preds.testdata <- predict(model.xgb,newdata = xgb.testdata)
data.to.deliver <- as.data.frame(cbind(original.testdata$id,preds.testdata))
names(data.to.deliver) <- c("id","claimcst0_pred")
write.csv(data.to.deliver,"Final_submission.csv")
