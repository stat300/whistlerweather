library(lattice)
library(latticeExtra)
library(zoo)

setwd("~/Documents/GitHub/whistlerweather/")
options(warn=-1)
rm(list=ls())
graphics.off()

load("Data/weather_data.Rdata")
#load("Data/winter.snow.Rdata")
#load("Data/winter.temp.Rdata")
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

#### BASIC PLOTS ####
mav <- function(x,n=10){filter(x,rep(1/n,n), sides=2)}
# Basic time series plot
# weather_data.ts <- zoo(weather_data[,c(5,9)], weather_data$date)
# names(weather_data.ts) = c("Average Temperature", 
#                            "Snow on ground")
# p <- xyplot(weather_data.ts,ylab=c("Snow on ground (cm)",
#                                    "Temperature (celsius)"))

weather_data.ts <- zoo(cbind(mav(imptemp,n=21), 
                             mav(impsnow,n=14)), weather_data$date)
names(weather_data.ts) = c("Average Temperature", 
                          "Snow on ground")
p <- xyplot(weather_data.ts,ylab=c("Snow on ground (cm)",
  "Temperature (celsius)"))
p

plot(season.avgsnow, type='l', ylim=c(0,max(season.maxsnow)),
     axes=FALSE, xlab="Date", ylab="Snow on ground (cm)")
lines(season.minsnow, type='l', col='red')
lines(season.maxsnow, type='l', col='blue')
axis(2)
box()
axis(1,at=c(30,120,210,300),labels=c("Feb","May","Aug","Nov"))
#plot(impsnow - rep(season.avgsnow,9),type='l')

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

# dual barplot
# barplot(matrix(c(t1,t2), 
#                nrow=2, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
#         col=c("black", "gray"), ylim=c(0,175))
# axis(1,at=seq(1.9,9*2.6,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
# box()
# legend('topleft', leg=c("Snowfall", "Temperature"),
#        col=c("black","gray"), bty="n", cex=0.8, ncol=3, pch=15)

# single barplot
barplot(matrix(c(t2), 
               nrow=1, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
        col=c("black"), ylim=c(0,175))
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
box()

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

# dual barplot
# barplot(matrix(c(snowfall_average.snow, snowfall_average.temp), 
#                nrow=2, byrow=TRUE), ylab="Average snow during winter", cex.axis = 0.8, beside=T,
#         col=c("black","gray"), ylim=c(0,100))
# axis(1,at=seq(1.9,9*2.6,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
# box()
# legend('topleft', leg=c("Snowfall", "Temperature"),
#        col=c("black","gray"), bty="n", cex=0.8, ncol=3, pch=15)

# single barplot
barplot(matrix(c(snowfall_average.snow), 
               nrow=1, byrow=TRUE), ylab="Average snow during winter", cex.axis = 0.8, beside=T,
        col=c("black"), ylim=c(0,100))
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)

average_dates <- function(dates) {
  dates <- as.POSIXlt(dates)
  for(i in 2:length(dates)) dates[i]$year <- dates[i]$year - (i-1)
  dates <- as.Date(dates)
  dates.avg <- mean(dates)
  dates <- rep(dates.avg,length(dates))
  #dates <- as.POSIXlt(dates)
  #for(i in 2:length(dates)) dates[i]$year <- dates[1]$year + (i-1)
  #as.Date(dates)
}

avgstart.snow <- average_dates(start_winter.snow.date)
avgstart.temp <- average_dates(start_winter.temp.date)
avgend.snow <- average_dates(end_winter.snow.date)
avgend.temp <- average_dates(end_winter.temp.date)
avgpeak.snow <- average_dates(peak_snow.date.snow)

# plot these start dates using pch Oct - Apr
# d1 <- as.POSIXlt(start_winter.snow.date)
# d2 <- as.POSIXlt(end_winter.snow.date)
# for(i in 2:length(d1)) {
#   d1[i]$year <- d1[i]$year - (i-1)
#   d2[i]$year <- d2[i]$year - (i-1)
# }
# 
# d1 <- as.Date(d1)
# d2 <- as.Date(d2)
# set.seed(10)
# r <- rep(c(seq(0.1,0.8,0.1)),2)
# plot(y=r, x=c(as.numeric(d1),as.numeric(d2)), pch='',
#      axes=FALSE, ylab="Year", xlab="Date")
# box()
# axis(1, at=c(min(d1),max(d2)),
#      labels=c("Nov 11", "April 13"))
# text(y=r, x=c(as.numeric(d1),as.numeric(d2)), labels=c(seq(2006,2013,1),
#                                                        seq(2007,2014,1)), cex= 0.7)

average_dates <- function(dates) {
  dates <- as.POSIXlt(dates)
  for(i in 2:length(dates)) dates[i]$year <- dates[i]$year - (i-1)
  dates <- as.Date(dates)
  dates.avg <- mean(dates)
}

avgstart.snow <- average_dates(start_winter.snow.date)
avgend.snow <- average_dates(end_winter.snow.date)
avgpeak.snow <- average_dates(peak_snow.date.snow)

library(lubridate)
start1 <- as.character(month(avgstart.snow, label = TRUE))
start2 <- as.character(day(avgstart.snow))
average.start <- paste(start1, start2)

end1 <- as.character(month(avgend.snow, label=TRUE))
end2 <- as.character(day(avgend.snow))
average.end <- paste(end1, end2)

peak1 <- as.character(month(avgpeak.snow, label=TRUE))
peak2 <- as.character(day(avgpeak.snow))
average.peak <- paste(peak1, peak2)

start1 <- as.character(month(start_winter.snow.date,label=TRUE))
start2 <- as.character(day(start_winter.snow.date))
start <- paste(start1, start2)

end1 <- as.character(month(end_winter.snow.date, label=TRUE))
end2 <- as.character(day(end_winter.snow.date))
end <- paste(end1, end2)

peak1 <- as.character(month(peak_snow.date.snow,label=TRUE))
peak2 <- as.character(day(peak_snow.date.snow))
peak <- paste(peak1, peak2)

length <- as.numeric(end_winter.snow.date - start_winter.snow.date)
average.length <- mean(length)

peak_amount <- peak_snow.snow
average.peak_amount <- mean(peak_amount)

summary_table <- data.frame(
  Winter=c("2006-2007","2007-2008",
           "2008-2009","2009-2010",
           "2010-2011","2011-2012",
           "2012-2013","2013-2014", "Average"),
  Start=c(start,average.start),
  End=c(end,average.end),
  Peak=c(peak,average.peak),
  Length=c(length,average.length),
  "Peak Amount"=c(peak_amount,average.peak_amount)
)

summary_table$Length <- round(summary_table$Length)
summary_table[,6] <- round(summary_table[,6])

save(summary_table,file="Data/summary_table.Rdata")
