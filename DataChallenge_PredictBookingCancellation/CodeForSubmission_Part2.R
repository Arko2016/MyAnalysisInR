#set path
path <- "C:/Users/Arko/Downloads/PrincessCruisessDataChallenge/"
setwd(path)
#check if relevant files are present in the directory
list.files()
#load libraries (install if not done before)
library(dplyr)
library(magrittr)
library(XLConnect)
library(ggplot2)

#load the data
wb = loadWorkbook("R Assessment_Retention Dataset.xlsx")
data = readWorksheet(wb, sheet = "Revenue Science Interview Reten", header = TRUE)

#Question 2:
#for removing missing values in AGE, we follow the same method as in part 1 of our analysis
data[(is.na(data$AGE) | data$AGE == 0) & data['BKNG_STATUS'] == "B",]$AGE <- median((data[data['BKNG_STATUS'] == "B",]$AGE),na.rm=T)
data[(is.na(data$AGE) | data$AGE == 0) & data['BKNG_STATUS'] == "C",]$AGE <- median((data[data['BKNG_STATUS'] == "C",]$AGE),na.rm=T)
#first let us create a new variable from META varaible having two values:
#people booking and not booking suites
data['META_coded'] <- ifelse(data$META %in% c("S","D"),"Suites","Nonsuites")
#we now filter out the data of people who are only Actively booking Suites
data.subset <- data %>% filter(BKNG_STATUS == 'B' & META_coded == "Suites")

#Now we plot the categorical variables vs the META_coded attribute to check patterns
#with AIR_FLAG
ggplot(data.subset, aes(AIR_FLAG, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: most of these people have not booked air with us
#with GROUP_TYPE 
ggplot(data.subset, aes(GROUP_TYPE, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: most of these people belong to Group G
#with BKNG_SOURCE
ggplot(data.subset, aes(BKNG_SOURCE, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: most of these people have booked through agency
#with BKNG_TYPE
ggplot(data.subset, aes(BKNG_TYPE, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: for most of these people preferred booking type is traditional independent booking
#with GENDER
ggplot(data.subset, aes(GENDER, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: there is not much difference between the two types of Gender 
#wrt to booking suites
#with DINING_TIME_CODE
ggplot(data.subset, aes(DINING_TIME_CODE, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#observation: most of these people prefer individual time for dining(group P)
#Inour previous analysis,we had created a new feature Resident_or_Immigrant indicating that
# if nationality = home_country, then resident(1) else immigrant(0)
#the intuition is that people may have passport issues for which they had to cancel
#the booking. Let's try to check its importance here as well
data.subset['Resident_or_Immigrant'] = as.factor(ifelse(data.subset$NATIONALITY == data.subset$HOME_COUNTRY,1,0))
ggplot(data.subset, aes(Resident_or_Immigrant, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: most of these people reside in their country of passport  

#we will bucket AGE into 4 groups based on the quantiles 
quantile(data.subset$AGE)
data.subset$agebins <- as.factor(findInterval(data.subset$AGE,
                                              c(quantile(data.subset$AGE)[0],
                                                quantile(data.subset$AGE)[1],
                                                quantile(data.subset$AGE)[2],
                                                quantile(data.subset$AGE)[3],
                                                quantile(data.subset$AGE)[4])))
ggplot(data.subset, aes(agebins, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: there is not much difference among the age groups in terms of
#booking suites
#NBR_CRUISES attribute is zero-inflated and hence might not prove to
#be a suitable indicator for depicting a pattern
quantile(data.subset$NET_TKT_REV)
data.subset$revenuebins <- as.factor(findInterval(data.subset$NET_TKT_REV,
                                              c(quantile(data.subset$NET_TKT_REV)[0],
                                                quantile(data.subset$NET_TKT_REV)[1],
                                                quantile(data.subset$NET_TKT_REV)[2],
                                                quantile(data.subset$NET_TKT_REV)[3],
                                                quantile(data.subset$NET_TKT_REV)[4])))
ggplot(data.subset, aes(revenuebins, ((..count..)/sum(..count..)))) + 
  geom_bar(aes(fill = META_coded), position = "dodge") +
  labs(y = "Percent")
#Observation: even in this case we are unable to find any distinguishing pattern
#Hence for these continuous variables, let us find the mean and median values
data.subset %>% summarize_at(vars(NET_TKT_REV,AGE),funs(mean,median))

#using these analysis and observations, we have a process to identify the characteristics of
#people who book suites (meta in ('S','D')) (Active bookings only)
#######################################################################
