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
# scatter.smooth(weather_data$snow_ground, type='l',
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
plot(pobj, xlab="Date", ylab="Mean temperature during day")
x <- as.numeric(weather_data$date)
model <- lm(weather_data$mean_temp[1:3285] ~ x)
abline(model, col='red')
pvalue <- coef(summary(model))[,4][2]
names(pvalue) <- "Date"
trend.coef <- coef(model)[2]
names(trend.coef) <- "Date"

# Basic time series plot
# weather_data.ts <- zoo(weather_data[,c(5,8,9)], weather_data$date)
# names(weather_data.ts) = c("Average Temperature", 
#                            "Total precipitation", "Snow on ground")
# xyplot(weather_data.ts,ylab=c("Snow on ground (cm)","Precipitation (cm)",
#                                    "Temperature (celsius)"))

# winter.snow
pobj <- zoo(winter.snow$weather_data[,c(12,13)], winter.snow$weather_data$date)
names(pobj) <- c("Average", "Peak")
p <- xyplot(pobj[winter.snow$weather_data$total_precip > 10], superpose=TRUE, ylab="Snowfall (cm)",
            auto.key=list(columns=2)) 
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[1,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[1,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[2,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[2,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[3,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[3,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[4,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[4,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[5,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[5,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[6,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[6,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[7,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[7,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[8,2] &
                                winter.snow$weather_data$date < winter.snow$winter_data[8,3], col = "lightblue"))
show(p)

# winter.temp
pobj <- zoo(winter.temp$weather_data[,c(12,13)], winter.temp$weather_data$date)
names(pobj) <- c("Average", "Peak")
p <- xyplot(pobj[winter.temp$weather_data$total_precip > 10], superpose=TRUE, ylab="Snowfall (cm)",
            auto.key=list(columns=2)) 
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[1,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[1,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[2,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[2,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[3,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[3,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[4,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[4,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[5,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[5,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[6,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[6,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[7,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[7,3], col = "lightblue"))
p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[8,2] &
                                winter.temp$weather_data$date < winter.temp$winter_data[8,3], col = "lightblue"))
show(p)

plot(season.avgsnow, type='l', ylim=c(0,max(season.maxsnow)),
     axes=FALSE, xlab="Date", ylab="Snow on ground (cm)")
lines(season.minsnow, type='l', col='red')
lines(season.maxsnow, type='l', col='blue')
axis(2)
box()
axis(1,at=c(30,120,210,300),labels=c("Feb","May","Aug","Nov"))

plot(impsnow - rep(season.avgsnow,9),type='l')
