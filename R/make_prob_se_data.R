setwd('~/shot_spotter/')

library(readr)
library(hpHawkes)
library(ggplot2)

library(data.table)
library(alphahull)
library(spatstat)
library(rgeos)
library(stringr)
#library(splancs)

source("R/utils.r")

set.seed(1)

# remove dec 29 - jan 2; july 1 - july 6
REMOVE_HOLIDAYS <- TRUE

# data1 = fread("data/WashingtonDC_Incidents_2006-2013_Raw_Data_2.csv")
# data1 = data1[,3:6,with=FALSE]
# setnames(data1,c("datetime","Type","long","lat"))
# data1 = data1[lat > 37]
# head(data1)
# 
# data1$date = as.Date(data1$datetime, format="%m/%d/%y")
# data1$timestamp = as.POSIXct(data1$datetime, format="%m/%d/%y %H:%M")
# data1$hour = format(data1$timestamp,"%H")
# data1$dow = format(data1$timestamp,"%u")
# data1$year = format(data1$timestamp,"%y")


data2 = fread("data/washington_dc_2006to2017.csv")
data2 = data2[data2$latitude > 37,]
data2 = data2[data2$year<2014,]
data2$second <- round(data2$second)
data2$datetime <- paste(data2$year,"-",data2$month,"-",data2$day, " ",
                        data2$hour, ":", data2$minute, ":",
                        data2$second,sep = "")
data2$timestamp = as.POSIXct(data2$datetime, format="%Y-%m-%d %H:%M:%OS")
data2$timestamp = format(data2$timestamp, "%m/%d/%y %H:%M:%OS")
data2 = data2[complete.cases(data2$timestamp),]
data2 = unique(data2)
dim(data2)

# remove holidays
data2$date = as.Date(data2$datetime, format="%Y-%m-%d")
doy = format(data2$date,format="%m-%d")
# code holidays as:
# July 2, 3, 4, 5, 6, December 29, 30, 31, January 1, 2

holidays = c("07-01","07-02", "07-03", "07-04", "07-05", "07-06", "12-29", "12-30", "12-31", "01-01", "01-02")
data2$holiday = doy %in% holidays

if (REMOVE_HOLIDAYS) {
  data2 <- data2[!data2$holiday,]
}
data2$time <- as.numeric(as.POSIXct(data2$datetime))
#data2$time = (data2$time - min(data2$time)) / 3600

t = data2$time
X = as.matrix(cbind(data2$latitude, data2$longitude))
Date <- as.Date(data2$date)
# X[,1] =(X[,1] - min(X[,1])) / 1000  # kilometers
# X[,2] =(X[,2] - min(X[,2])) / 1000 # kilometers

#################################################################
# get 2017-2019
data3 = fread("data/Shot_Spotter_Gun_Shots.csv")
#data3 <- data3[data3$TYPE=="Single_Gunshot"|data3$TYPE=="Multiple_Gunshots",]
data3$DATETIME <- stringr::str_replace(data3$DATETIME,"T", " ")
data3$DATETIME <- stringr::str_replace(data3$DATETIME,".000Z", "")
data3$timestamp = as.POSIXct(data3$DATETIME, format="%Y-%m-%d %H:%M:%OS")
data3$timestamp = format(data3$timestamp, "%m/%d/%y %H:%M:%OS")
data3 = data3[complete.cases(data3$timestamp),]
data3 = unique(data3)

# remove holidays
data3$date = as.Date(data3$DATETIME, format="%Y-%m-%d")
#data3$year <- format(data3$date,"%y")
doy = format(data3$date,format="%m-%d")
# code holidays as:
# July 2, 3, 4, 5, 6, December 29, 30, 31, January 1, 2

holidays = c("07-01","07-02", "07-03", "07-04", "07-05", "07-06", "12-29", "12-30", "12-31", "01-01", "01-02")
data3$holiday = doy %in% holidays

if (REMOVE_HOLIDAYS) {
  data3 <- data3[!data3$holiday,]
}
data3$time <- as.numeric(as.POSIXct(data3$DATETIME))
data3$Dates <- as.Date(data3$date)
t <- c(t,data3$time)
Date <- c(Date,data3$Date)

X = rbind(X, as.matrix(cbind(data3$LATITUDE, data3$LONGITUDE)) )



plot(X)
df = data.frame(X=X[,1], Y=X[,2], Time=t, Date = Date)
df <- df[order(df$Time),]
dim(df)
df <- unique(df)

load("data/prob_child.Rdata")
df$Probabilities <- prob_child1
colnames(df) <- c("Longitude","Latitude", "Time","Probabilities")
df2 <- df[,c(4,5)]
df <- df[,c(1,2,5)]

#write.csv(df,file = "data/argis_data.csv",row.names = FALSE)
save(df,file="data/dc_locations_and_self_excit_probs.Rdata")


save(df2,file="data/dates_and_self_excit_probs.Rdata")



