#load the required packages
library(dplyr)
library(magrittr)
#set the working directory
setwd("C:/Users/Arko/Downloads/Studies/PersonalResearch/RDataChallenge/")
#read the data
data1 <- read.csv("NEISS2014.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
data2 <- read.csv("BodyParts.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
data3 <- read.csv("DiagnosisCodes.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
data4 <- read.csv("Disposition.csv",header = TRUE,stringsAsFactors = TRUE,na.strings = c(""))
#join the child datasets to parent dataset
data <- left_join(data1,data2,by = c("body_part" = "Code"))
data <- left_join(data,data3,by = c("diag" = "Code"))
data <- left_join(data,data4,by = c("disposition" = "Code"))
#remove the initial datasets
rm(data1,data2,data3,data4)
#What are the top three body parts most frequently represented in this dataset?
top3.bodyparts <- data %>% 
                  select(BodyPart) %>%
                  filter(!grepl("not recorded",tolower(BodyPart))) %>%
                  group_by(BodyPart) %>%
                  summarise(FrequencyBodyParts = n()) %>% 
                  arrange(desc(FrequencyBodyParts)) %>%
                  head(3)
#What are the top three body parts that are least frequently represented?
bottom3.bodyparts <- data %>%
                     select(BodyPart) %>%
                     filter(!grepl("not recorded",tolower(BodyPart))) %>%
                     group_by(BodyPart) %>%
                     summarise(FrequencyBodyParts = n()) %>%
                     arrange(FrequencyBodyParts) %>%
                     head(3)
#How many injuries in this dataset involve a skateboard?
involve.skateboard.count <- data %>%
                          filter(grepl("skateboard",tolower(narrative))) %>% 
                          summarise(Count = n())
#Of those injuries, what percentage were male and what percentage were female?
involve.skateboard.gender <- data %>%
                      filter(grepl("skateboard",tolower(narrative))) %>%
                      group_by(sex) %>%
                      summarise(Count = n()) %>%
                      select(sex,Count)
#What was the average age of someone injured in an incident involving a skateboard?
involve.skateboard.avgage <- data %>%
                      filter(grepl("skateboard",tolower(narrative))) %>% 
                      summarise(Avgage = round(mean(age,na.rm=T),2))
#What diagnosis had the highest hospitalization rate? 
diag.highesthosp <- data %>%
                    select(Diagnosis) %>%
                    group_by(Diagnosis) %>%
                    summarise(Count = n()) %>%
                    arrange(desc(Count)) %>%
                    head(1)
#What diagnosis most often concluded with the individual leaving without being seen?
diag.withoutseen <- data %>%
                    filter(grepl("left without being seen",tolower(Disposition))) %>%
                    filter(!grepl("other/not stated",tolower(Diagnosis))) %>%
                    select(Diagnosis) %>%
                    group_by(Diagnosis) %>%
                    summarise(Count = n()) %>%
                    arrange(desc(Count)) %>%
                    head(1)
