data <- read.csv("C:/Users/Arko/Downloads/Studies/PersonalResearch/NetworkProject/Fortune500_WRDSdata.csv",
                 header = T)
#############################TEST CODE ON SAMPLE DATA##################
samp.data <- head(data,20)
samp.data$OverlapYearEnd <- unfactor(samp.data$OverlapYearEnd)
library(dplyr)
samp.data <- select(samp.data, CIKCode,BoardName,DirectorName,OverlapYearStart,OverlapYearEnd)
samp.data$OverlapYearEnd <- ifelse(samp.data$OverlapYearEnd == "Curr",2017,samp.data$OverlapYearEnd)
#samp.data$OverlapYearEnd <- as.Date(as.character(samp.data$OverlapYearEnd),"%Y")
#samp.data$OverlapYearStart <- as.Date(as.character(samp.data$OverlapYearStart),"%Y")
samp.data$gap <- samp.data$OverlapYearEnd - samp.data$OverlapYearStart
#separate the data with gap = 0
data.sameyear <- filter(samp.data,gap == 0)
data.sameyear$DirectorName_Assoctiation <- paste0(data.sameyear$DirectorName,"-Assoc",
                                                  data.sameyear$OverlapYearStart)
data.sameyear$gap <- NULL
library(splitstackshape)
#xyz <- setDT(expandRows(samp.data, "gap"))[,
#                                 gap := sprintf("%s-%s-%s%d", BoardName, DirectorName, "s",1:.N) , DirectorName][]
data.multiyears <- setDT(expandRows(samp.data, "gap"))[,
          gap := sprintf("%s-%s%d", DirectorName,"Assoc",OverlapYearStart+(1:.N)) , DirectorName][]
names(data.multiyears)[names(data.multiyears)=="gap"] = "DirectorName_Assoctiation"
#rbind both dataframes
final.data <- rbind(data.sameyear,data.multiyears)
######################TEST CODE ENDS##################################

data$OverlapYearEnd <- as.numeric(as.character(data$OverlapYearEnd))
data$OverlapYearEnd[is.na(data$OverlapYearEnd)] = 2017
data$OverlapYearStart[is.na(data$OverlapYearStart)] = 2017
library(dplyr)
data <- select(data, CIKCode,BoardName,DirectorName,OverlapYearStart,OverlapYearEnd)
#get the #years of association
data$gap <- data$OverlapYearEnd - data$OverlapYearStart
#separate the data with gap = 0
data.sameyear <- filter(data,gap == 0)
data.sameyear$DirectorName_Assoctiation <- paste0(data.sameyear$BoardName,"-Assoc",
                                                  data.sameyear$OverlapYearStart)
data.sameyear$gap <- NULL
library(splitstackshape)
#xyz <- setDT(expandRows(samp.data, "gap"))[,
#                                 gap := sprintf("%s-%s-%s%d", BoardName, DirectorName, "s",1:.N) , DirectorName][]
data.multiyears <- filter(data,gap != 0)
#expand rows with multiple years of association
data.multiyears <- setDT(expandRows(data, "gap"))[,
                                                       gap := sprintf("%s-%s%d", BoardName,"Assoc",OverlapYearStart+(1:.N)) , DirectorName][]
names(data.multiyears)[names(data.multiyears)=="gap"] = "DirectorName_Assoctiation"
#rbind both dataframes
final.data <- rbind(data.sameyear,data.multiyears)
final.data$CIK.Board <- paste0(final.data$CIKCode,"_",final.data$DirectorName_Assoctiation)
data.to.wrtite <- select(final.data,CIK.Board,DirectorName)
write.csv(data.to.wrtite,"Fortune500_WRDS_onlyboardsandmembers.csv")





