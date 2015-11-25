library(lattice)
library(latticeExtra)
library(zoo)

setwd("~/Documents/GitHub/whistlerweather/")
options(warn=-1)
rm(list=ls())
graphics.off()

load("Data/weather_data.Rdata")
load("Data/winter.snow.Rdata")
load("Data/winter.temp.Rdata")
load("Data/average_ts.Rdata")

# Trend

##  Snow on ground
pobj <- zoo(weather_data$snow_ground, weather_data$date)
#scatter.smooth(weather_data$snow_ground, type='l',
#                ylab="snow ground (cm)")
plot(pobj, xlab="Date", ylab="Snow on ground (cm)")
x <- as.numeric(weather_data$date)
model <- lm(weather_data$snow_ground[1:3285] ~ x)
abline(model,col='red')
pvalue <- coef(summary(model))[,4][2]
names(pvalue) <- "Date"
trend.coef <- coef(model)[2]
names(trend.coef) <- "Date"

## Temperature
pobj <- zoo(weather_data$mean_temp, weather_data$date)
#scatter.smooth(weather_data$mean_temp, type='l')
plot(pobj, xlab="Date", ylab="Mean temperature during day")
x <- as.numeric(weather_data$date)
model <- lm(weather_data$mean_temp[1:3285] ~ x)
abline(model, col='red')
pvalue <- coef(summary(model))[,4][2]
names(pvalue) <- "Date"
trend.coef <- coef(model)[2]
names(trend.coef) <- "Date"

# Basic time series plot
weather_data.ts <- zoo(weather_data[,c(5,9)], weather_data$date)
names(weather_data.ts) = c("Average Temperature", 
                           "Snow on ground")
xyplot(weather_data.ts,ylab=c("Snow on ground (cm)",
                                   "Temperature (celsius)"))

plot(season.avgsnow, type='l', ylim=c(0,max(season.maxsnow)),
     axes=FALSE, xlab="Date", ylab="Snow on ground (cm)")
lines(season.minsnow, type='l', col='red')
lines(season.maxsnow, type='l', col='blue')
axis(2)
box()
axis(1,at=c(30,120,210,300),labels=c("Feb","May","Aug","Nov"))

plot(impsnow - rep(season.avgsnow,9),type='l')

barplot(matrix(c(winter.snow$winter_data$length_winter, winter.temp$winter_data$length_winter), 
               nrow=2, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
        col=c("black", "gray"), ylim=c(0,175))
axis(1,at=seq(1.9,9*2.6,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
box()
legend('topleft', leg=c("Snowfall", "Temperature"),
       col=c("black","gray"), bty="n", cex=0.8, ncol=3, pch=15)


mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

x <- ts(mav(imptemp, 7))
y <- diff(which(x<2.5))
t1 <- diff(which(y>150))

# date when moving average goes below 2.5
start_winter.temp <- (which(x<2.5)[which(y>150)+1]-3)[1:8]
end_winter.temp  <- start_winter.temp + t1
start_winter.temp.date <- weather_data$date[start_winter.temp]
end_winter.temp.date <- weather_data$date[end_winter.temp]

x <- ts(mav(impsnow, 7))
y <- diff(which(x>10))
t2 <- diff(which(y>60))

# date when moving average goes above 10
start_winter.snow <- (which(x>10)[which(y>60)+1]-3)[1:8]
end_winter.snow <- start_winter.snow + t2
start_winter.snow.date <- weather_data$date[start_winter.snow]
end_winter.snow.date <- weather_data$date[end_winter.snow]

barplot(matrix(c(t1,t2), 
               nrow=2, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
        col=c("black", "gray"), ylim=c(0,175))
axis(1,at=seq(1.9,9*2.6,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
box()
legend('topleft', leg=c("Snowfall", "Temperature"),
       col=c("black","gray"), bty="n", cex=0.8, ncol=3, pch=15)

snowfall_average.snow <- numeric(8)
snowfall_average.temp <- numeric(8)
peak_snow.snow <- numeric(8)
peak_snow.date.snow <- as.Date(rep("2000-01-01",8))

for(i in 1:8) {
  c <-  (weather_data$date > start_winter.snow.date[i]) * 
        (weather_data$date <= end_winter.snow.date[i])
  snow <- weather_data$snow_ground[which(c==1)]
  snowfall_average.snow[i] <- mean(snow[!is.na(snow)])
  peak_snow.snow[i] <- max(snow[!is.na(snow)])
  
  tmp <- weather_data[which(c==1),]
  peak_snow.date.snow[i] <- tmp$date[which(tmp$snow==peak_snow.snow[i])][1]
  
  c <-  (weather_data$date > start_winter.temp.date[i]) * 
        (weather_data$date <= end_winter.temp.date[i])
  snow <- weather_data$snow_ground[which(c==1)]
  snowfall_average.temp[i] <- mean(snow[!is.na(snow)])
}

barplot(matrix(c(snowfall_average.snow, snowfall_average.temp), 
               nrow=2, byrow=TRUE), ylab="Average snow during winter", cex.axis = 0.8, beside=T,
        col=c("black","gray"), ylim=c(0,100))
axis(1,at=seq(1.9,9*2.6,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
box()
legend('topleft', leg=c("Snowfall", "Temperature"),
       col=c("black","gray"), bty="n", cex=0.8, ncol=3, pch=15)

average_dates <- function(dates) {
  dates <- as.POSIXlt(dates)
  for(i in 2:length(dates)) dates[i]$year <- dates[i]$year - (i-1)
  dates <- as.Date(dates)
  dates.avg <- mean(dates)
  dates <- as.POSIXlt(rep(dates.avg,length(dates)))
  for(i in 2:length(dates)) dates[i]$year <- dates[1]$year + (i-1)
  as.Date(dates)
}

avgstart.snow <- average_dates(start_winter.snow.date)
avgstart.temp <- average_dates(start_winter.temp.date)
avgend.snow <- average_dates(end_winter.snow.date)
avgend.temp <- average_dates(end_winter.temp.date)
avgpeak.snow <- average_dates(peak_snow.date.snow)

# plot these dates using pch Oct - Apr
