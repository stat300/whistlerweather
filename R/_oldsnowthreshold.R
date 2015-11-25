# analyze winter using snow threshold
library(zoo)
thres <- 10

setwd("~/Documents/GitHub/whistlerweather/")
load("Data/weather_data.Rdata")

weather_data$average <- NA
weather_data$max <- NA
winter_data <- as.data.frame(setNames(object=replicate(9, character(0), simplify=F), 
  nm = c("winter", "start","end","peak","peak_value","average_value","length_winter",
         "largest_melt", "average_temp")))
  
nwinters <- round(length(weather_data[,1])/365) - 1
for(i in 1:nwinters) {
  # July 1 to July 1 
  season <- weather_data[which((paste0(2005+i,"-07-01") <= weather_data$date) * 
    (weather_data$date < paste0(2006+i,"-07-01")) == 1),]
  season.snow <- season$snow_ground
  season.meantemp <- season$mean_temp
  season.peaksnow <- max(season$snow_ground[!is.na(season$snow_ground)])
  season.peakdate <- season$date[which(season$snow_ground == season.peaksnow)][1]
  
  # snowfall measure
  ## imputed snow for NA
  season.impsnow <- season.snow
  season.impsnow[1] <- 0
  for(j in 2:length(season.impsnow)) {
    if(is.na(season.impsnow[j])) 
      season.impsnow[j] = season.impsnow[j-1]
  }

  season.snowperiod <- as.numeric(season.impsnow > thres) 
  season.length <- 0
  season.startindex <- 0
  season.endindex <- 0
  for(j in 1:length(season.snowperiod)) {
    if(season.snowperiod[j]==1) {
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
  season.tempinseason <- season.meantemp[season.startindex:season.endindex]
  
  season.startdate  <- season$date[season.startindex]
  season.enddate <- season$date[season.endindex]
  season.averagesnow <- mean(season.snowinseason[!is.na(season.snowinseason)])
  season.averagetemp <- mean(season.tempinseason[!is.na(season.tempinseason)])
  
  # analyze largest amount of snow which melted (false winter)
  season.snownotinseason <- season.snow[c(1:(season.startindex-1),(season.endindex+1):length(season.snow))]
  season.largestmelt <- max(season.snownotinseason[!is.na(season.snownotinseason)])
  
  # add information to winter_data
  winter_data <- rbind(winter_data, 
                       data.frame(winter=i, start=season.startdate,
                                  end=season.enddate, peak=season.peakdate,
                                  peak_value=season.peaksnow, average_value=season.averagesnow,
                                  length_winter=season.length, largest_melt=season.largestmelt,
                                  average_temp=season.averagetemp))
  
  startindex <- which(weather_data$date == season.startdate)
  endindex <- which(weather_data$date == season.enddate)
  weather_data$average[startindex:endindex] <- season.averagesnow
  weather_data$max[startindex:endindex] <- season.peaksnow
}

winter.snow <- list(weather_data=weather_data, winter_data=winter_data)
save(winter.snow, file="Data/winter.snow.Rdata")