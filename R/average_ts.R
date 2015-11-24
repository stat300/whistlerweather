library(zoo)

rm(list=ls())
graphics.off()
cat("\014")
setwd("~/Documents/GitHub/whistlerweather/")
load("Data/weather_data.Rdata")
load("Data/winter.snow.Rdata")
load("Data/winter.temp.Rdata")

weather_data.ts <- zoo(weather_data[,3:11], weather_data$date)

analyze_winter <- function(winter_data) {
  # Determine average of dates
  season.startdate <- as.POSIXlt(winter_data$start) 
  season.enddate <- as.POSIXlt(winter_data$end)
  season.peakdate <- as.POSIXlt(winter_data$peak)
  
  for(i in 2:length(season.startdate)) {
    season.startdate[i]$year <- season.startdate[i]$year - (i-1)
    season.enddate[i]$year <- season.enddate[i]$year - (i-1)
    season.peakdate[i]$year <- season.peakdate[i]$year - (i-1)
  }
  
  season.startdate <- as.Date(season.startdate)
  season.avgstartdate <- mean(season.startdate)
  season.enddate <- as.Date(season.enddate)
  season.avgenddate <- mean(season.enddate)
  season.peakdate <- as.Date(season.peakdate)
  season.avgpeakdate <- mean(season.peakdate)
  
  season.avgstartdate <- as.POSIXlt(rep(season.avgstartdate,
                             length(season.startdate)))
  season.avgenddate <- as.POSIXlt(rep(season.avgenddate,
                              length(season.enddate)))
  season.avgpeakdate <- as.POSIXlt(rep(season.avgpeakdate,
                                length(season.peakdate)))
  
  for(i in 2:length(season.avgstartdate)) {
    season.avgstartdate[i]$year <- season.avgstartdate[1]$year + (i-1)
    season.avgenddate[i]$year <- season.avgenddate[1]$year + (i-1)
    season.avgpeakdate[i]$year <- season.avgpeakdate[1]$year + (i-1)
  }
  
  season.avgstartdate <- as.Date(season.avgstartdate)
  season.avgenddate <- as.Date(season.avgenddate)
  season.avgpeakdate <- as.Date(season.avgpeakdate)
  
  list(avgstart=season.avgstartdate, 
       avgend=season.avgenddate,
       avgpeak=season.avgpeakdate)
}

avgdate.snow <- analyze_winter(winter.snow$winter_data)
avgdate.temp <- analyze_winter(winter.temp$winter_data)

# derive the average time series (for snow on ground)
season.index <- seq(1,3285-364,365)
season.all <- data.frame(i=1:365)
for(i in 2:9) {
  season.all <- cbind(season.all,
  weather_data$snow_ground[season.index[i-1]:(season.index[i]-1)])
}
season.all <- cbind(season.all,
                        weather_data$snow_ground[(365*8+1):(365*9)])
names(season.all) <- c("i",seq(2006,2014,1))
season.all <- season.all[,-1]

# impute
season.impall <- season.all
season.impall[1,2] <- season.impall[364,1]
season.impall[1,6] <- season.impall[361,5]
season.impall[1,9] <- season.impall[361,8]
for(i in 1:9) {
  for(j in 2:365) {
    if(is.na(season.impall[j,i])) 
      season.impall[j,i] <- season.impall[j-1,i]
      if(j>10) {
        if(all(season.impall[(j-10):j,i] == season.impall[j,i]))
          season.impall[j,i] <- 0
      }
  }
}

season.impall[355:358,5] <- season.impall[354,5]

season.avgsnow <- numeric(365)
season.minsnow <- numeric(365)
season.maxsnow <- numeric(365)
for(i in 1:365) {
  day <- as.numeric(season.impall[i,])
  season.avgsnow[i] <- mean(day)
  season.minsnow[i] <- min(day)
  season.maxsnow[i] <- max(day)
}

impsnow <- as.numeric(as.matrix(season.impall))

# derive the average time series (for temperature)
season.index <- seq(1,3285-364,365)
season.all <- data.frame(i=1:365)
for(i in 2:9) {
  season.all <- cbind(season.all,
    weather_data$mean_temp[season.index[i-1]:(season.index[i]-1)])
}
season.all <- cbind(season.all,
                    weather_data$mean_temp[(365*8+1):(365*9)])
names(season.all) <- c("i",seq(2006,2014,1))
season.all <- season.all[,-1]

# impute
season.impall <- season.all
season.impall[1,2] <- season.impall[364,1]
for(i in 1:9) {
  for(j in 2:365) {
    if(is.na(season.impall[j,i])) 
      season.impall[j,i] <- season.impall[j-1,i]
  }
}
season.avgtemp <- numeric(365)
season.mintemp <- numeric(365)
season.maxtemp <- numeric(365)
for(i in 1:365) {
  day <- as.numeric(season.impall[i,])
  season.avgtemp[i] <- mean(day)
  season.mintemp[i] <- min(day)
  season.maxtemp[i] <- max(day)
}

imptemp <- as.numeric(as.matrix(season.impall))

save(avgdate.snow,avgdate.temp,
     impsnow, season.minsnow, season.maxsnow, season.avgsnow,
     imptemp, season.mintemp, season.maxtemp, season.avgtemp,
     file="Data/average_ts.Rdata")
