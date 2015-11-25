# winter.snow
# pobj <- zoo(winter.snow$weather_data[,c(12,13)], winter.snow$weather_data$date)
# names(pobj) <- c("Average", "Peak")
# p <- xyplot(pobj[winter.snow$weather_data$total_precip > 10], superpose=TRUE, ylab="Snowfall (cm)",
#             auto.key=list(columns=2)) 
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[1,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[1,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[2,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[2,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[3,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[3,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[4,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[4,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[5,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[5,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[6,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[6,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[7,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[7,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.snow$weather_data$date, winter.snow$weather_data$date > winter.snow$winter_data[8,2] &
#                                 winter.snow$weather_data$date < winter.snow$winter_data[8,3], col = "lightblue"))
# show(p)

# winter.temp
# pobj <- zoo(winter.temp$weather_data[,c(12,13)], winter.temp$weather_data$date)
# names(pobj) <- c("Average", "Peak")
# p <- xyplot(pobj[winter.temp$weather_data$total_precip > 10], superpose=TRUE, ylab="Snowfall (cm)",
#             auto.key=list(columns=2)) 
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[1,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[1,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[2,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[2,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[3,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[3,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[4,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[4,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[5,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[5,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[6,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[6,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[7,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[7,3], col = "lightblue"))
# p <- p + layer_(panel.xblocks(winter.temp$weather_data$date, winter.temp$weather_data$date > winter.temp$winter_data[8,2] &
#                                 winter.temp$weather_data$date < winter.temp$winter_data[8,3], col = "lightblue"))
# show(p)