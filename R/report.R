rm(list=ls())
graphics.off()
cat("\014")

thres <- 15
library(lattice)
library(latticeExtra)
library(zoo)
load("~/Documents/GitHub/whistlerweather/Data/weather_data.Rdata")
load("~/Documents/GitHub/whistlerweather/Data/average_ts.Rdata")
load(file="~/Documents/GitHub/whistlerweather/Data/summary_table.Rdata")

#### overview plot ####
# weather_data.ts <- zoo(weather_data[,c(5,9)], weather_data$date)
# names(weather_data.ts) = c("Average Temperature", 
#                            "Snow on ground")
# p <- xyplot(weather_data.ts,ylab=c("Snow on ground (cm)",
#                                    "Temperature (celsius)"))
# show(p)
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

# plot(mav(season.avgsnow,n=14), type='l', ylim=c(0,max(season.maxsnow)),
#      axes=FALSE, xlab="Date", ylab="Snow on ground (cm)")
# lines(mav(season.minsnow,n=14), type='l', col='red')
# lines(mav(season.maxsnow,n=14), type='l', col='blue')
# axis(2)
# box()
# axis(1,at=c(30,120,210,300),labels=c("Feb","May","Aug","Nov"))

#### length of winter plot ####
mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}

x <- ts(mav(impsnow, 7))
y <- diff(which(x>thres))
t2 <- diff(which(y>60))
if(thres>14) {
  t2 <- c(t2, tail(which(x>thres),1) - tail(which(x>thres)[which(y>60)+1]-3,1))
}

# robust check: thres = 10, 15, 20
thres <- 10
x <- ts(mav(impsnow, 7))
y <- diff(which(x>thres))
t2 <- diff(which(y>60))
if(thres>14) t2 <- c(t2, tail(which(x>thres),1) - tail(which(x>thres)[which(y>60)+1]-3,1))
robust_table <- data.frame(matrix(c(thres,t2),nrow=1))

thres <- 15
x <- ts(mav(impsnow, 7))
y <- diff(which(x>thres))
t2 <- diff(which(y>60))
if(thres>14) t2 <- c(t2, tail(which(x>thres),1) - tail(which(x>thres)[which(y>60)+1]-3,1))
robust_table <- rbind(robust_table, data.frame(matrix(c(15,t2),nrow=1)))

thres <- 20
x <- ts(mav(impsnow, 7))
y <- diff(which(x>thres))
t2 <- diff(which(y>60))
if(thres>14) t2 <- c(t2, tail(which(x>thres),1) - tail(which(x>thres)[which(y>60)+1]-3,1))
robust_table <- rbind(robust_table, data.frame(matrix(c(thres,t2),nrow=1)))

names(robust_table) <- c("Threshold", "06-07", "07-08", "08-09", "09-10",
                         "10-11", "11-12", "12-13", "13-14")

start_winter.snow <- (which(x>thres)[which(y>60)+1]-3)[1:8]
end_winter.snow <- start_winter.snow + t2
start_winter.snow.date <- weather_data$date[start_winter.snow]
end_winter.snow.date <- weather_data$date[end_winter.snow]

#barplot(matrix(c(t2), 
#               nrow=1, byrow=TRUE), ylab="Length of winter", cex.axis = 0.8, beside=T,
#        col=c("black"), ylim=c(0,175))

par(mfrow=c(2,2))
plot(t2-6,type='h', lwd=30, lend="square", ylim=c(0,155),
     ylab="Length of winter (days)", xlab="Winter season (2000's)", axes=FALSE)
axis(2,at=seq(-6,144,50),labels=c(0,50,100,150))
lines(t2)
box()
axis(1,at=1:8,labels=c("06-07","07-08","08-09",
                              "09-10","10-11","11-12",
                              "12-13","13-14"))
#x <- 1:8
#lo <- loess(t2~x)
#lines(predict(lo),type='l',lty=2)
#axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)

#### average snow plot ###
snowfall_average.snow <- numeric(8)
peak_snow.snow <- numeric(8)
peak_snow.date.snow <- as.Date(rep("2000-01-01",8))
temperature_average.snow <- numeric(8)

for(i in 1:8) {
  c <-  (weather_data$date > start_winter.snow.date[i]) * 
    (weather_data$date <= end_winter.snow.date[i])
  snow <- weather_data$snow_ground[which(c==1)]
  temp <- weather_data$mean_temp[which(c==1)]
  snowfall_average.snow[i] <- mean(snow[!is.na(snow)])
  temperature_average.snow[i] <- mean(temp[!is.na(temp)])
  peak_snow.snow[i] <- max(snow[!is.na(snow)])
  tmp <- weather_data[which(c==1),]
  peak_snow.date.snow[i] <- tmp$date[which(tmp$snow==peak_snow.snow[i])][1]
}

barplot(matrix(c(snowfall_average.snow), 
               nrow=1, byrow=TRUE), ylab="Average snow during winter", cex.axis = 0.8, beside=T,
        col=c("black"), ylim=c(0,100))
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)
box()

### average temperature plot ####
barplot(matrix(c(temperature_average.snow), 
               nrow=1, byrow=TRUE), ylab="Average temperature during winter", cex.axis = 0.8, 
        beside=T, col=c("black"), ylim=c(-2.5,0))
axis(1,at=seq(1.55,9.5*1.63,length.out = 8), labels=seq(2007,2014,1), cex.axis=0.8)

cor(temperature_average.snow, snowfall_average.snow)

#### TABLE ####
summary_table
