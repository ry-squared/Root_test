library(geosphere)
library(ggmap)
library(graphics)

# load data and format timestamps
csv_file <- "/Users/ryanwest/Downloads/data_analyst_work_sample/data/sample_trip.csv"
sample_trip <- read.csv(file = csv_file, stringsAsFactors = FALSE, row.names = 1)
sample_trip$timestamp <- as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")

#NA check
for (i in 1:nrow(sample_trip)){
  if (any(is.na(sample_trip[i,]))){
    print(paste0("NA rows: ",i))
  }
}

#masssive speed difference before and after NA
sample_trip[890:910,]

#calculate time and speed differences
sample_trip$time_diff <- c(0,diff(as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")))
sample_trip$speed_diff <- c(0, diff(sample_trip$speed))

#visualize consecutive timestamp differences
plot(sample_trip$time_diff)

#isolate row with negative timestamp difference
negative_row <- as.numeric(rownames(sample_trip[sample_trip$time_diff == -1,]))

#view data surrounding suspicious timestamp
#likely faulty timestamp at row 400- timestamp looks like it "jumps ahead" 3 seconds and then "falls back" 1 second at next row
sample_trip[(negative_row-10):(negative_row+10),]

#impute reasonable timestamp for suspicious row
sample_trip[negative_row-1,]$timestamp <- sample_trip[negative_row-2,]$timestamp + 1

#recalculate time differences and plot time_diff as sanity check
sample_trip$time_diff <- c(0,diff(as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")))
plot(sample_trip$time_diff)

#observations 110, 1033, and 1051 also have unexpected timestamp diff
#upon further inspection, there simply appears to be missing records given that speed is roughly the same surounding data points 
#keep these data points in mind when calculating accelaration
sample_trip[100:120,]
sample_trip[1020:1065,]

#unequal time diffs at end of dataframe may be indicator that trip has ended, especially since associated velocities are all 0's
sample_trip[1447:nrow(sample_trip),]

#could employ Chebyshev's Theorem to automate outlier detection, but ouliers are quite obvious here
#lat,lon outliers and more than 12 std deviations from mean
plot(sample_trip$latitude, xlab = "index", ylab = "Lat", main = "Lattitude vs. index")
abline(h = mean(sample_trip$latitude) - 6*sd(sample_trip$latitude), col = "red")
abline(h = mean(sample_trip$latitude) - 12*sd(sample_trip$latitude), col = "blue")
legend(1200, 39.9, legend=c("6 std deviations", "12 std deviations"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

plot(sample_trip$longitude, xlab = "index", ylab = "Lon", main = "Longitude vs. index")
abline(h = mean(sample_trip$longitude) + 6*sd(sample_trip$longitude), col = "red")
abline(h = mean(sample_trip$longitude) + 12*sd(sample_trip$longitude), col = "blue")
legend(1200, -82.3, legend=c("6 std deviations", "12 std deviations"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


#fix lat, lon with linear interpolation between bad points
sample_trip$latitude[sample_trip$latitude < mean(sample_trip$latitude) - 12*sd(sample_trip$latitude)] <- NA
sample_trip$longitude[sample_trip$longitude > mean(sample_trip$longitude) + 12*sd(sample_trip$longitude)] <- NA
sample_trip$latitude[is.na(sample_trip$latitude)] <- approx(x = sample_trip$timestamp, y = sample_trip$latitude, xout = sample_trip$timestamp[is.na(sample_trip$latitude)])$y
sample_trip$longitude[is.na(sample_trip$longitude)] <- approx(x = sample_trip$timestamp, y = sample_trip$longitude, xout = sample_trip$timestamp[is.na(sample_trip$longitude)])$y

#calculate distances between consecutive lat/lon using spherical/ellipsoid earth models
lon <- sample_trip$longitude
lat <- sample_trip$latitude
dist_vince <- vector()
dist_haver <- vector()
for (i in 1:(nrow(sample_trip)-1)){
  dist_haver[i] <- geosphere::distHaversine(c(lon[i],lat[i]),c(lon[i+1],lat[i+1]))
  dist_vince[i] <- geosphere::distVincentyEllipsoid(c(lon[i],lat[i]),c(lon[i+1],lat[i+1]))
}

#there is very little difference between spherical vs ellipsoid estimates of distances between lat/lon
#the range of differences between estimates is at most .15 meters, which is negible considering uncertainy (accuracy) is an order of magnitude or more larger than .15 meters
plot(dist_haver-dist_vince)

#estimates are well within range of another
vince_cum_dist <- sum(dist_vince)*(1/1609.34)
haversine_cum_dist <- sum(dist_haver)*(1/1609.34)

#calculate distances using speed and time differences
dist_speed_time <- vector()
for (i in 1:(nrow(sample_trip)-1)){
  dist_speed_time[i] <- sample_trip$speed[i]*sample_trip$time_diff[i+1]
}
#cumulative distance is much lower than estimates using GPS coordinates!
speed_time_cum_dist <- sum(dist_speed_time, na.rm = TRUE)*(1/1609.34)

#Let's do some error propogation and add accuracy values in quadrature
#Unclear if accuracy values are associated with GPS points or speed
estimated_dist_error <- sqrt(sum((sample_trip$accuracy)**2))*(1/1609.34)

#results don't agree even when propogating error- must be a systematic bias somewhere
#Let's smooth out speed using the (edited) fix_speed mock function, i.e. interpolate NA's and innacurately recorded speeds
sample_trip$smooth_speed <- fix_speed(sample_trip$timestamp,
                                      sample_trip$speed,
                                      sample_trip$accuracy)

#recalculate distance using smoothed speed and time diffs
#calculate distances using speed and time differences
dist_smooth_speed_time <- vector()
for (i in 1:(nrow(sample_trip)-1)){
  dist_smooth_speed_time[i] <- sample_trip$smooth_speed[i]*sample_trip$time_diff[i+1]
}
smooth_speed_time_cum_dist <- sum(dist_smooth_speed_time)*(1/1609.34)

#plot results for calculated distances using smoothed speed, haversine, and vince
plot(x = 1:4, y = c(speed_time_cum_dist,smooth_speed_time_cum_dist, haversine_cum_dist,vince_cum_dist), xaxt = "n", xlab = "", ylab = "total trip distance in miles", xlim = c(1,5), ylim = c(7.0,8.5))
axis(1, at=1:5, labels=c("Raw (unsmoothed) distance","Smoothed distance","Haversine distance","Vince distance", "Uncertainty"))
legend(3.5, 7.8, legend=paste0("estimation error = ", round(estimated_dist_error,1), " miles"), lty=1:2, cex=0.8, col = "red")
avg <- mean(c(smooth_speed_time_cum_dist,vince_cum_dist,haversine_cum_dist))
error <- estimated_dist_error
arrows(5, avg-error, 5, avg+error, length=0.05, angle=90, code=3, col = "red")

tail(sample_trip, 50)
#car stops moving at row 1434
total_time <- sum(sample_trip$time_diff[1:1434])
# total trip duration ~ 1439 seconds

#plotting coordinates gives better intuition of map
cbus <- get_map(location = c(lon = mean(sample_trip$longitude), lat = mean(sample_trip$latitud)), zoom = 12, maptype = "satellite", scale = 2)
ggmap(cbus) +
  geom_point(data = as.data.frame(cbind(sample_trip$longitude,sample_trip$latitude)), aes(x = sample_trip$longitude, y = sample_trip$latitude, fill = "red", alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#headings plot is noisy and headings aren't intuitive 
plot(sample_trip$heading)

#ajustment function to make headings plot easier to interpret
adjust <- function(x){
  if (x > 150){
    x <- x -360
  }
  return(x)
}

#filter headings vector
#"heading" typically point north when car is stopped or nearly stopped, so let's filter out stops
#poor "accuracy" can also result in innaccurate heading
speed_thresh <- 1.5
plot(cbind(lapply(as.vector(sample_trip$heading[sample_trip$smooth_speed > speed_thresh & sample_trip$accuracy < 25]),adjust)), ylab = "heading")

#looks like 10 turns greater than 60 degrees- select turns from plot
time_index <- round(locator(10)$x)

#last timestamp corresponds to a U-turn, so it is counted as a single BIG turn rather than two right turns
sample_trip[sample_trip$smooth_speed > speed_thresh & sample_trip$accuracy < 25,][time_index,c(3,4,6)]

#approximate location/timestamp of 10 turns > 60 degrees:
# latitude longitude           timestamp
# 400  39.98793 -83.02563 2018-01-17 18:36:51
# 649  39.99755 -83.02457 2018-01-17 18:41:00
# 692  39.99993 -83.02920 2018-01-17 18:41:43
# 847  40.02743 -83.03575 2018-01-17 18:44:18
# 882  40.03254 -83.02804 2018-01-17 18:44:53
# 1067 40.06312 -83.03114 2018-01-17 18:48:01
# 1317 40.06395 -83.05816 2018-01-17 18:52:11
# 1342 40.06506 -83.05828 2018-01-17 18:52:36
# 1358 40.06503 -83.05891 2018-01-17 18:52:52
# 1402 40.06469 -83.05914 2018-01-17 18:53:36


