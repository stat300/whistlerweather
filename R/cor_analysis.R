setwd("~/Documents/GitHub/whistlerweather/")
options(warn=-1)
rm(list=ls())
graphics.off()

load("Data/weather_data.Rdata")
load("Data/winter.snow.Rdata")
load("Data/winter.temp.Rdata")
load("Data/average_ts.Rdata")

cor(winter.snow$winter_data$average_temp, winter.snow$winter_data$average_value)
cor(winter.snow$winter_data$average_temp, winter.snow$winter_data$peak_value)
cor(winter.snow$winter_data$average_temp, winter.snow$winter_data$length_winter)
