rm(list=ls())
graphics.off()
cat("\014")

choice_thres <- 15
library(lattice)
library(latticeExtra)
library(zoo)
load("~/Documents/GitHub/whistlerweather/Data/weather_data.Rdata")
load("~/Documents/GitHub/whistlerweather/Data/average_ts.Rdata")

#### overview plot ####
mav <- function(x,n=10){filter(x,rep(1/n,n), sides=2)}
weather_data.ts <- zoo(cbind(mav(imptemp,n=21), 
                             mav(impsnow,n=14)), weather_data$date)
names(weather_data.ts) = c("Average Temperature", 
                           "Snow on ground")
p <- xyplot(weather_data.ts,ylab=c("Snow on ground (cm)",
                                   "Temperature (celsius)"))
p

#### snow on ground trend ####
pobj <- zoo(mav(impsnow,n=14), weather_data$date)
plot(pobj, xlab="Date", ylab="Snow on ground (cm)")
x <- as.numeric(weather_data$date)
snowmodel <- lm(weather_data$snow_ground[1:3285] ~ x)
abline(snowmodel,col='red')

#### temperature trend ####
# pobj <- zoo(weather_data$mean_temp, weather_data$date)
# plot(pobj, xlab="Date", ylab="Mean temperature during day")
# x <- as.numeric(weather_data$date)
# model <- lm(weather_data$mean_temp[1:3285] ~ x)
# abline(model, col='red')

#### average, min, max ####
splitsnow.avg <- c(season.avgsnow[181:365],season.avgsnow[1:180])
splitsnow.min <- c(season.minsnow[181:365],season.minsnow[1:180])
splitsnow.max <- c(season.maxsnow[181:365],season.maxsnow[1:180])

plot(mav(splitsnow.avg,n=14), type='l', ylim=c(0,max(season.maxsnow)+10),
     axes=FALSE, xlab="Date", ylab="Snow on ground (cm)")
lines(mav(splitsnow.min,n=14), type='l', col='red')
lines(mav(splitsnow.max,n=14), type='l', col='blue')
axis(2)
box()
axis(1,at=c(30,120,210,300),labels=c("Aug 1", "Nov 1", "Feb 1",
                                     "May 1"))
legend('top', leg=c("Maximum", "Average", "Minimum"),
       col=c('blue', 'black', 'red'), lty=c(1,1,1), ncol=3)

#### length of winter plot ####
mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

snow_index <- ts(mav(impsnow, 7))
nosnow_period <- diff(which(snow_index>choice_thres))
length_winter <- diff(which(nosnow_period>60))
if(choice_thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
        tail(which(snow_index>choice_thres),1) - 
        tail(which(snow_index>choice_thres)[which(nosnow_period>60)+1]-3,1))
}

# robust check: thres = 10, 15, 20
thres <- 10
nosnow_period <- diff(which(snow_index>thres))
length_winter <- diff(which(nosnow_period>60))
if(thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
        tail(which(snow_index>thres),1) - 
        tail(which(snow_index>thres)[which(nosnow_period>60)+1]-3,1))
}
robust_table <- data.frame(matrix(c(thres,length_winter),nrow=1))

thres <- 15
nosnow_period <- diff(which(snow_index>thres))
length_winter <- diff(which(nosnow_period>60))
if(thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
        tail(which(snow_index>thres),1) - 
        tail(which(snow_index>thres)[which(nosnow_period>60)+1]-3,1))
}
robust_table <- rbind(robust_table, data.frame(matrix(c(15,length_winter),nrow=1)))

thres <- 20
nosnow_period <- diff(which(snow_index>thres))
length_winter <- diff(which(nosnow_period>60))
if(thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
        tail(which(snow_index>thres),1) - 
        tail(which(snow_index>thres)[which(nosnow_period>60)+1]-3,1))
}
robust_table <- rbind(robust_table, data.frame(matrix(c(thres,length_winter),nrow=1)))

names(robust_table) <- c("Threshold", "06-07", "07-08", "08-09", "09-10",
                         "10-11", "11-12", "12-13", "13-14")

thres <- choice_thres
nosnow_period <- diff(which(snow_index>thres))
length_winter <- diff(which(nosnow_period>60))
if(thres>tail(snow_index[!is.na(snow_index)],1)) {
  length_winter <- c(length_winter, 
          tail(which(snow_index>thres),1) - 
          tail(which(snow_index>thres)[which(nosnow_period>60)+1]-3,1))
}
start_winter <- (which(snow_index>choice_thres)[which(nosnow_period>60)+1]-3)[1:8]
end_winter <- start_winter + length_winter
start_winter.date <- weather_data$date[start_winter]
end_winter.date <- weather_data$date[end_winter]

#barplot(matrix(c(length_winter), 
#               nrow=1, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
#        col=c("black"), ylim=c(0,175))

# par(mfrow=c(2,2))
# plot(t2-6,type='h', lwd=30, lend="square", ylim=c(0,155),
#      ylab="Length of winter (days)", xlab="Winter season (2000's)", axes=FALSE)
# axis(2,at=seq(-6,144,50),labels=c(0,50,100,150))
# lines(t2)
# box()
# axis(1,at=1:8,labels=c("06-07","07-08","08-09",
#                               "09-10","10-11","11-12",
#                               "12-13","13-14"))

#### average snow plot ###
average_snow <- numeric(8)
peak_snow <- numeric(8)
peak_snow.date <- as.Date(rep("2000-01-01",8))
average_temperature <- numeric(8)

for(i in 1:8) {
  c <-  (weather_data$date > start_winter.date[i]) * 
    (weather_data$date <= end_winter.date[i])
  snow <- weather_data$snow_ground[which(c==1)]
  temp <- weather_data$mean_temp[which(c==1)]
  average_snow[i] <- mean(snow[!is.na(snow)])
  average_temperature[i] <- mean(temp[!is.na(temp)])
  peak_snow[i] <- max(snow[!is.na(snow)])
  
  tmp <- weather_data[which(c==1),]
  peak_snow.date[i] <- tmp$date[which(tmp$snow==peak_snow[i])][1]
}

barplot(matrix(c(average_snow), 
               nrow=1, byrow=TRUE), ylab="Average snow during winter", cex.axis = 0.8, beside=T,
        col=c("black"), ylim=c(0,100), xlab="Winter season")
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=c("06-07","07-08","08-09",
                                                      "09-10","10-11","11-12",
                                                      "12-13","13-14"), cex.axis=0.8)
box()

### average temperature plot ####
barplot(matrix(c(average_temperature), 
               nrow=1, byrow=TRUE), ylab="Average temperature during winter", cex.axis = 0.8, 
        beside=T, col=c("black"), ylim=c(-2.5,0))
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)

cor(average_temperature, average_snow)

#### TABLE ####
average_dates <- function(dates) {
  dates <- as.POSIXlt(dates)
  for(i in 2:length(dates)) dates[i]$year <- dates[i]$year - (i-1)
  dates <- as.Date(dates)
  dates.avg <- mean(dates)
}

average_start.date <- average_dates(start_winter.date)
average_end.date <- average_dates(end_winter.date)
average_peak.date <- average_dates(peak_snow.date)

library(lubridate)
average_start.date <- paste(as.character(month(average_start.date, label = TRUE)), 
                       as.character(day(average_start.date)))
average_end.date <- paste(as.character(month(average_end.date, label=TRUE)), 
                     as.character(day(average_end.date)))
average_peak.date <- paste(as.character(month(average_peak.date, label=TRUE)), 
                      as.character(day(average_peak.date)))
start.date <- paste(as.character(month(start_winter.date,label=TRUE)), 
               as.character(day(start_winter.date)))
end.date <- paste(as.character(month(end_winter.date,label=TRUE)), 
                  as.character(day(end_winter.date)))
peak.date <- paste(as.character(month(peak_snow.date,label=TRUE)), 
                  as.character(day(peak_snow.date)))

summary_table <- data.frame(
  Winter=c("2006-2007","2007-2008",
           "2008-2009","2009-2010",
           "2010-2011","2011-2012",
           "2012-2013","2013-2014", "Average"),
  Start=c(start.date,average_start.date),
  End=c(end.date,average_end.date),
  Length=c(length_winter,mean(length_winter)),
  Peak=c(peak.date,average_peak.date),
  "Peak Amount"=c(peak_snow,mean(peak_snow)),
  Average=c(average_snow,mean(average_snow))
)

summary_table$Length <- round(summary_table$Length)
summary_table[,6] <- round(summary_table[,6])
summary_table[,7] <- round(summary_table[,7])
