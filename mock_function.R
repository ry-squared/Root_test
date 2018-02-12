# Mock function
library(dplyr)
library(zoo)
library(forecast)

rds_file <- "/Users/ryanwest/Downloads/data_analyst_work_sample/data/sample_trip.rds"
sample_trip = readRDS(rds_file)

#clean data- same manipulation code from previous file, now as a function
#while rds data is different than csv data, exploratory analysis showed datasets had many of the same characteristics, so similar processing steps could be applied
clean_data <- function(sample_trip = sample_trip){
  
  #format timestamps
  sample_trip$timestamp <- as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")
  
  #calculate time and speed differences
  sample_trip$time_diff <- c(0,diff(as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")))
  
  #isolate row with negative timestamp difference
  negative_row <- as.numeric(rownames(sample_trip[sample_trip$time_diff == -1,]))
  
  #impute reasonable timestamp for suspicious row
  sample_trip[negative_row-1,]$timestamp <- sample_trip[negative_row-2,]$timestamp + 1
  
  #recalculate time differences
  sample_trip$time_diff <- c(0,diff(as.POSIXct(sample_trip$timestamp, origin = "1970-01-01")))
  
  #fix lat, lon with linear interpolation between bad points
  sample_trip$latitude[sample_trip$latitude < mean(sample_trip$latitude) - 12*sd(sample_trip$latitude)] <- NA
  sample_trip$longitude[sample_trip$longitude > mean(sample_trip$longitude) + 12*sd(sample_trip$longitude)] <- NA
  sample_trip$latitude[is.na(sample_trip$latitude)] <- approx(x = sample_trip$timestamp, y = sample_trip$latitude, xout = sample_trip$timestamp[is.na(sample_trip$latitude)])$y
  sample_trip$longitude[is.na(sample_trip$longitude)] <- approx(x = sample_trip$timestamp, y = sample_trip$longitude, xout = sample_trip$timestamp[is.na(sample_trip$longitude)])$y
  
  return(sample_trip)
}

sample_trip <- clean_data(sample_trip)

#Original code was equivalent to taking rolling moving average, but only if all time_steps = 1
#included two additional types of smoothing, i.e. simple exponential and moving average
get_acceleration = function(time_vect, speed_vect, lag = 1, type = "default") {
  dt = as.numeric(diff(time_vect))
  dv = diff(speed_vect)
  #moving average where window of average = lag
  if (type == "ma"){
    accel = zoo::rollmean(dv/dt, k = lag)
  }
  #exponentially weighted moving average
  else if (type == "ewma"){
    accel = as.vector(stats::fitted(stats::arima(dv/dt, order = c(0,1,1))))
  }
  #default calculation
  else if (type == "default"){
    dt = as.numeric(diff(time_vect, lag = lag))
    dv = diff(speed_vect, lag = lag)
    accel = dv/dt
  }
  else{
    return("type must be set to 'ma', 'ewma', or 'default")
  }
  return(accel)
}

#changed function to linearly interpolate between intervals of bad points
#original function linearly interpolates from first bad index to last bad index
fix_speed = function(time_vect, speed_vect, 
                     accuracy_vect, accuracy_thresh = 25) {
  bad_accuracy = accuracy_vect > accuracy_thresh | is.na(speed_vect) #NA speeds are bad, too
  #start_bad = min(which(bad_accuracy))
  #end_bad = max(which(bad_accuracy))
  speed_vect[bad_accuracy] = NA
  new_speed = approx(x = time_vect,
                     y = speed_vect,
                     xout = time_vect[bad_accuracy])$y
  speed_vect[bad_accuracy] = new_speed
  return(speed_vect)
}

sample_trip = sample_trip %>% 
  mutate(smooth_speed = fix_speed(timestamp, speed, accuracy))

summary = sample_trip %>%
  summarize(max_speed = max(speed, na.rm = TRUE),
            max_accel_lag1 = get_acceleration(timestamp, smooth_speed, lag = 1) %>% max,
            max_accel_lag2 = get_acceleration(timestamp, smooth_speed, lag = 2) %>% max,
            max_accel_lag3 = get_acceleration(timestamp, smooth_speed, lag = 3) %>% max,
            max_accel_ma = get_acceleration(timestamp, smooth_speed, type = "ma") %>% max,
            max_accel_ma_lag2 = get_acceleration(timestamp, smooth_speed, lag = 2, type = "ma") %>% max,
            max_accel_ewma = get_acceleration(timestamp, smooth_speed, type = "ewma") %>% max
  )

#Quick google search shows 'fastest' cars accelerate to 60 mph in 3 seconds, or ~8.94 m/s^2
#https://en.wikipedia.org/wiki/List_of_fastest_production_cars_by_acceleration
#Using a 1 lagged model likely does not accurately capture the acceleration
#Additionally, exponential smoothing might smooth over too much
print(summary)

#visualize default acceleration against other methods
strt <- 1400
stp <- 1474
plot(get_acceleration(sample_trip$timestamp, sample_trip$smooth_speed, lag = 1, type = "default")[strt:stp], xlab = "time index", ylab = "accel", main = "Acceration over time")
lines(get_acceleration(sample_trip$timestamp, sample_trip$smooth_speed, lag = 1, type = "ma")[strt:stp], col = "blue")
lines(get_acceleration(sample_trip$timestamp, sample_trip$smooth_speed, type = "ewma")[strt:stp], col = "green")

