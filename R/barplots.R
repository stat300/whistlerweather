rm(list=ls())
load("~/Documents/GitHub/whistlerweather/Data/summary.Rdata")
load("~/Documents/GitHub/whistlerweather/Data/weather_data.Rdata")
load("~/Documents/GitHub/whistlerweather/Data/average_ts.Rdata")

mav <- function(x,n=10){filter(x,rep(1/n,n), sides=2)}

snow_index <- ts(mav(impsnow, 7))
choice_thres <- 15
thres <- choice_thres
nosnow_period <- diff(which(snow_index>thres))
length_winter <- diff(which(nosnow_period>60))
if(thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
                     tail(which(snow_index>thres),1) - 
                       tail(which(snow_index>thres)[which(nosnow_period>60)+1]-3,1))
}
start_winter <- (which(snow_index>thres)[which(nosnow_period>60)+1]-3)[1:8]
end_winter <- start_winter + length_winter

#plot(impsnow,type='l')
bars_snow <- rep(NA, length(impsnow))
bars_temp <- rep(NA, length(imptemp))
for(i in 1:8) {
  bars_snow[start_winter[i]:end_winter[i]] <- average_snow[i]
  bars_temp[start_winter[i]:end_winter[i]] <- average_temperature[i]
}

bars <- zoo(bars_snow, weather_data$date)
plot(bars, type='h', col='gray', ylim=c(min(impsnow), max(impsnow)),
     ylab=c("Snow on ground (cm)"), xlab="Date")
lines(zoo(mav(impsnow,n=7), weather_data$date),type='l')

# bars <- zoo(bars_temp, weather_data$date)
# plot(bars, col='gray', ylim=c(min(imptemp), max(imptemp)),
#      ylab=c("Temperature"), xlab="Date")
# lines(zoo(mav(imptemp,n=7), weather_data$date),type='l')
