#links
#https://rpubs.com/bradleyboehmke/data_wrangling
#http://stcorp.nl/R_course/tutorial_dplyr.html
#https://rstudio-pubs-static.s3.amazonaws.com/114838_c5798ba6f60d4ed099ff58043a3f3d7e.html

library(dplyr)
library(tidyr)
library(magrittr)
data <- data.frame(temperature = runif(3, 15, 25), 
                   rain_station1 = runif(3, 1, 3), 
                   rain_station2 = runif(3, 1, 3),
                   rain_station3 = runif(3, 1, 3))
#gather function is used to reshape the data in such a way that
#there is one column containing station ids and the other containing 
#amount of rainfall received
#Hence, reshaping wide format to long format
#gather() -> data, new key, new value and the columns from Existing dataframe
#to be used to fill the new key and value columns
data.gather <- data %>% gather(station,rainfall,rain_station1:rain_station3)
#same as
data.gather <- data %>% gather(station,rainfall,rain_station1,rain_station2,rain_station3)
#spread is the opposite of gather()
#it will redistribute the columns to have individual rain_station columns
#Hence, reshaping long format to wide format
#spread() -> data, existing key and value column, fill option to fill empty values
data.gather %<>% spread(station,rainfall,fill = 0)
#this will make the changes inplace
#to create separate dataframe
data.spread <- data.gather %>% spread(station,rainfall,fill = 0)
#create two or more column from an existing column
#separate() -> data,existing column, name of columns created on separation, separator
data.separate <- data.gather %>% separate(station,c('station_type','station_id'),sep="_")
#merging two variables into one with an optional separator
#unite() -> data, name of new merged column, existing columns, separator
data.unite <- data.separate %>% unite(station,station_type,station_id,sep = '.')
