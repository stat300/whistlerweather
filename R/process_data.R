# Legend
# A:    Accumulated
# C:    Precipitation occured, amount uncertain
# E:    Estimated
# F:    Accumulated and estimated
# L:    Precipitation may or may not have occurred
# M:    Missing
# N:    Temperature missing but known to be > 0
# S:    More than one occurrence
# T:    Trace
# Y:    Temperature missing but known to be > 0
# [empty]:  No data available
# ^:    The value displayed is based on incomplete data
# 1:    Data for this day has undergone only preliminary quality checking
# 0:    Partner data that is not subject to review by the National Climate Archives

setwd("~/Documents/GitHub/whistlerweather/")
weather_2006 <- read.csv("raw/weather_12312006.csv")
weather_2007 <- read.csv("raw/weather_12312007.csv")
weather_2008 <- read.csv("raw/weather_12312008.csv")
weather_2009 <- read.csv("raw/weather_12312009.csv")
weather_2010 <- read.csv("raw/weather_12312010.csv")
weather_2011 <- read.csv("raw/weather_12312011.csv")
weather_2012 <- read.csv("raw/weather_12312012.csv")
weather_2013 <- read.csv("raw/weather_12312013.csv")
weather_2014 <- read.csv("raw/weather_12312014.csv")
weather_data <- rbind(weather_2006, weather_2007, weather_2008, 
                      weather_2009, weather_2010, weather_2011,
                      weather_2012, weather_2013, weather_2014)
weather_data$Data.Quality <- as.character(weather_data$Data.Quality)
weather_data$Data.Quality[weather_data$Data.Quality!=""] = "G"
weather_data$Data.Quality[weather_data$Data.Quality==""] = "B"
weather_data <- weather_data[,-c(2,3,4)] # remove redundant date cols

rm(weather_2006, weather_2007, weather_2008, weather_2009,
   weather_2010, weather_2011, weather_2012, weather_2013,
   weather_2014)

# remove unneeded flags
wdn <- names(weather_data)
flags_index <- c(which(wdn=="Max.Temp.Flag"), 
                 which(wdn=="Min.Temp.Flag"),
                 which(wdn=="Mean.Temp.Flag"),
                 which(wdn=="Heat.Deg.Days.Flag"), 
                 which(wdn=="Cool.Deg.Days.Flag"),
                 which(wdn=="Total.Rain.Flag"),
                 which(wdn=="Total.Snow.Flag"),
                 which(wdn=="Total.Precip.Flag"),
                 which(wdn=="Snow.on.Grnd.Flag"),
                 which(wdn=="Dir.of.Max.Gust.Flag"),
                 which(wdn=="Spd.of.Max.Gust.Flag"))
# remove cols which are all missing
rain_snow_index <- c(which(wdn=="Total.Rain..mm."), 
                     which(wdn=="Total.Snow..cm."))
weather_data <- weather_data[,-c(flags_index,rain_snow_index)]

weather_data$Spd.of.Max.Gust..km.h. <- as.character(weather_data$Spd.of.Max.Gust..km.h.)
weather_data$Spd.of.Max.Gust..km.h.[which(weather_data$Spd.of.Max.Gust..km.h.== "<31")] <- 0

for(i in 3:length(weather_data[1,])) {
    weather_data[,i] <- as.numeric(as.character(weather_data[,i]))
}
names(weather_data) <- c("date", "quality", "max_temp", "min_temp", "mean_temp",
                         "heat", "cool", "total_precip", "snow_ground", "dir_max_gust",
                         "spd_max_gust")

rm(wdn,rain_snow_index,i,flags_index)
weather_data$date <- as.character(weather_data$date)
weather_data$date <- as.Date(weather_data$date, format="%m/%d/%Y")
# str(weather_data)

weather_data <- weather_data[-c(790,2251),] # leap year
row.names(weather_data) <- 1:length(weather_data[,1])
save.image(file="Data/weather_data.Rdata")
