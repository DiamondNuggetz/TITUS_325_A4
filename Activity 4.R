library(dplyr)
library(lubridate)
library(ggplot2)

## Make sure time zones match between data.
## Present/interpret data in local time

weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = c("#N/A","NA"))

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")

weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz = "America/New_York")
weatherCheck <- weather %>%
  filter(is.na(weather$dateET))
weather$dateET

weather$dateF[2] %--% weather$dateF[3]
int_length(weather$dateF[2] %--% weather$dateF[3]) #default unit = seconds
test <- weather$dateF[1:10]
test
test[-1] #Removes certain number of observations from beginning

# x is a date vector
##PROMPT 3
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
} 

timeCheck900(weather$dateF)

soilFiles <- list.files("/cloud/project/activity04/soil/")
#Setting up variable to be used in for loop
soilList <-list()

for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}

str(soilList)

soilData <- do.call("rbind", soilList)
View(soilData)

#Calculate Moving Average
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA <- airMA

weatherJan <- weather %>%
  filter(year(dateET) == 2022 & month(dateET) == 1)

ggplot(weatherJan, aes(x=dateET))+
  geom_line(aes(x=dateET, y=AirTemp, color="AirTemp"))+
  geom_line(aes(x=dateET,y=airMA, color="airMA"))+
  scale_color_manual(name="", values=c("airMA"="blue", "AirTemp"="red"),labels=c("AirTemp"="Instant Air Temperature", "airMA"="2-hour Moving Average Temperature"))+
  xlab("Date")+
  ylab("Air Temperature")

#PROMPT 2
weatherMJ <- weather %>%
  filter(year(dateET) == 2021 & month(dateET) == 5 | month(dateET) == 6 | month(dateET) == 4 | month(dateET) == 7)

ggplot(weatherMJ, aes(x=dateET, y=SolRad))+
  geom_line()


##HOMEWORK

#Prompt 1
weatherPrecip <- weather %>%
  filter(AirTemp >= 0 & XLevel <= 2 & YLevel <= 2) 