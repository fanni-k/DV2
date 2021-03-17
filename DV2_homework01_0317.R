
library(data.table)
library(ggplot2)
library(dplyr)

# load nycflights13 - flights data set
flight_data <- nycflights13::flights

# check the data type -> it is data frame
class(flight_data) 

# convert data frame into data table -> not it is data table
flight_data <- as.data.table(flight_data)
class(flight_data)

# check the structure
str(flight_data)

# Which destination had the lowest avg arrival delay from LGA with minimum 100 flight to that destination?
flight_data[origin == "LGA" & flight >= 100, list(origin, avg_arr_delay = mean(arr_delay, na.rm = TRUE), dest, flight), by = dest][order(avg_arr_delay)]
# dest = LEX with -22 average arrival delay

# Which destination's flights were the most on time (avg arrival delay closest to zero) from LGA with minimum 100 flight to that destination?
flight_data[origin == "LGA" & flight >= 100, list(origin, avg_arr_delay_abs_val = abs(mean(arr_delay, na.rm = TRUE)), dest, flight), by = dest][order(avg_arr_delay_abs_val)]
# dest = MIA with 0.0657662 average arrival delay

# Who is the manufacturer of the plane, which flights the most to CHS destination?
planes <- as.data.table(nycflights13::planes)
planes_manu <- merge(flight_data, planes[, list(manufacturer), by = tailnum], by = 'tailnum')
planes_manu[dest == "CHS", list(.N, manufacturer), by = tailnum][order(N)]
# manufacturer = BOMBARDIER INC with 22 flights

# Which airline (carrier) flow the most by distance?
flight_data[, .(sum_of_distance = sum(distance), carrier), by = carrier][order(sum_of_distance)]
carrier = UA

# Plot the monthly number of flights with 20+ mins arrival delay!
plot(flight_data[arr_delay > 20, .N, by = month])

# Plot the departure delay of flights going to IAH and the related day's wind speed on a scaterplot!
# Is there any association between the two variables? Try adding a linear model.

# weather <-as.data.table(nycflights13::weather)

# filter on dest = IAH
# flight_IAH <- flight_data[dest== 'IAH', .(dest, origin, dep_delay, time_hour)]

# create a key from time_hour&origin
# flight_IAH[, ID := paste(flight_IAH$origin, flight_IAH$time_hour)]
# weather[, ID := paste(weather$origin, weather$time_hour)]

# merge is unsuccessful because IDs are not unique 
# merge(flight_data_from_IAH, weather[, .(ID, time_hour, wind_speed), by = ID], by = 'ID')
# flight_IAH <- flight_IAH[, nu_ID := .N, by = ID]
#  weather <- weather[, num_ID := .N, by = ID]

# let's create two tables where we have average wind speed by origin
# weather_EWR  
  # weather[origin == 'EWR', list(hourly = .N, date = paste(year, month, day), daily_avg_wind = mean(wind_speed, na.rm = TRUE))]

  
# Plot the airports as per their geolocation on a world map, 
# by mapping the number flights going to that destination to the size of the symbol!
airports <- as.data.table(nycflights13::airports)
setnames(airports, "faa", "dest")

dest_fligh_nu <- flight_data[, .N, by = dest]
dest_fligh_nu <- merge(dest_fligh_nu, airports, by = "dest")

install.packages("ggmap")
library("ggmap")
library(maptools)
library(maps)
mapWorld <- borders("world", colour="gray50", fill="white")
mp <- ggplot() + mapWorld
dest_fligh_nu_plot <- mp + geom_point(data = dest_fligh_nu, aes(x = lon, y = lat, size = N))
dest_fligh_nu_plot
