# analyze winter using temperature threshold
library(zoo)
thres <- 3.5

setwd("~/Documents/GitHub/whistlerweather/")
load("Data/weather_data.Rdata")

weather_data$average <- NA
weather_data$max <- NA
winter_data <- as.data.frame(setNames(object=replicate(8, character(0), simplify=F), 
  nm = c("winter", "start","end","peak","peak_value","average_value","length_winter","largest_melt")))
  
nwinters <- round(length(weather_data[,1])/365) - 1
for(i in 1:nwinters) {
  # July 1 to July 1 
  season <- weather_data[which((paste0(2005+i,"-07-01") <= weather_data$date) * 
    (weather_data$date < paste0(2006+i,"-07-01")) == 1),]
  season.snow <- season$snow_ground
  season.peaksnow <- max(season$snow_ground[!is.na(season$snow_ground)])
  season.peakdate <- season$date[which(season$snow_ground == season.peaksnow)][1]
  
  # mean temperature measure
  ## imputed temperature for NA
  season.imptemperature <- season$mean_temp
  season.imptemperature[1] <- ifelse(is.na(season.imptemperature[1]),20,season.imptemperature[1])
  for(j in 2:length(season.imptemperature)) {
    if(is.na(season.imptemperature[j])) 
      season.imptemperature[j] = season.imptemperature[j-1]
  }
  season.tempperiod <- as.numeric(season.imptemperature < thres) 
  season.length <- 0
  season.startindex <- 0
  season.endindex <- 0
  for(j in 1:length(season.tempperiod)) {
    if(season.tempperiod[j]==1) {
      if(season.startindex==0) {
        season.startindex <- j
      } else {
        if(season.endindex==0) {
          season.length <- season.length + 1
        }
      }
    } else {
      if(season.length < 40) { # reset: false winter
        season.length <- 0
        season.startindex <- 0
      } else { # end of winter
        if(season.endindex == 0) {
          season.endindex <- j
        }
      }
    }
  }
  season.snowinseason <- season.snow[season.startindex:season.endindex]
  season.startdate  <- season$date[season.startindex]
  season.enddate <- season$date[season.endindex]
  season.averagesnow <- mean(season.snowinseason[!is.na(season.snowinseason)])
  
  # analyze largest amount of snow which melted (false winter)
  season.snownotinseason <- season.snow[c(1:(season.startindex-1),(season.endindex+1):length(season.snow))]
  season.largestmelt <- max(season.snownotinseason[!is.na(season.snownotinseason)])
  
  # add information to winter_data
  winter_data <- rbind(winter_data, 
                       data.frame(winter=i, start=season.startdate,
                                  end=season.enddate, peak=season.peakdate,
                                  peak_value=season.peaksnow, average_value=season.averagesnow,
                                  length_winter=season.length, largest_melt=season.largestmelt))
  
  startindex <- which(weather_data$date == season.startdate)
  endindex <- which(weather_data$date == season.enddate)
  weather_data$average[startindex:endindex] <- season.averagesnow
  weather_data$max[startindex:endindex] <- season.peaksnow
}

winter.temp <- list(weather_data=weather_data, winter_data=winter_data)
save(winter.temp, file="Data/winter.temp.Rdata")