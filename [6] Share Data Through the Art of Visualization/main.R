# Step 1: Setting up environment in R
library(tidyverse)
library(lubridate) # handles dates and times 
library(ggplot2)
library(data.table)

# Step 2: Import the csv file
tripdata_2022_01 <- read_csv("C:/Users/USER/Downloads/Downloads/202201-divvy-tripdata.csv")
tripdata_2022_02 <- read_csv("C:/Users/USER/Downloads/Downloads/202201-divvy-tripdata.csv")
tripdata_2022_03 <- read_csv("C:/Users/USER/Downloads/Downloads/202203-divvy-tripdata.csv")
tripdata_2022_04 <- read_csv("C:/Users/USER/Downloads/Downloads/202204-divvy-tripdata.csv")
tripdata_2022_05 <- read_csv("C:/Users/USER/Downloads/Downloads/202205-divvy-tripdata.csv")
tripdata_2022_06 <- read_csv("C:/Users/USER/Downloads/Downloads/202206-divvy-tripdata.csv")
tripdata_2022_07 <- read_csv("C:/Users/USER/Downloads/Downloads/202207-divvy-tripdata.csv")
tripdata_2022_08 <- read_csv("C:/Users/USER/Downloads/Downloads/202208-divvy-tripdata.csv")
tripdata_2022_09 <- read_csv("C:/Users/USER/Downloads/Downloads/202209-divvy-tripdata.csv")
tripdata_2022_10 <- read_csv("C:/Users/USER/Downloads/Downloads/202210-divvy-tripdata.csv")
tripdata_2022_11 <- read_csv("C:/Users/USER/Downloads/Downloads/202211-divvy-tripdata.csv")
tripdata_2022_12 <- read_csv("C:/Users/USER/Downloads/Downloads/202212-divvy-tripdata.csv")

# Explore the dataset (before cleaning the data)
View(tripdata_2022_12)
colnames(tripdata_2022_01)
unique(tripdata_2022_01$start_station_name)
str(tripdata_2022_01)
head(tripdata_2022_01)
dim(tripdata_2022_01)
summary(tripdata_2022_01)

# Combine many lists into a single list using bind_rows()
all_tripdata <- bind_rows(tripdata_2022_01, tripdata_2022_02, tripdata_2022_03, tripdata_2022_04,
                          tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08,
                          tripdata_2022_09, tripdata_2022_10, tripdata_2022_11, tripdata_2022_12)
typeof(all_tripdata)

# Column that includes null values
colnames(all_tripdata)[colSums(is.na(all_tripdata)) > 0]
# Since we have many data, I will remove Rows with NA
all_tripdata <- na.omit(all_tripdata)
# Check again if it does not have null values
colnames(all_tripdata)[colSums(is.na(all_tripdata)) > 0]

# Create a column called "ride_length" by subtracting the column "started_at" from the column "ended_at"
# In this case, start and end time is within a day, so I don't take the date for the calculation

# Find difference (return in seconds)
ride_length_diff <- difftime(all_tripdata$ended_at, all_tripdata$started_at)
ride_length_ms <- round((ride_length_diff / 3600 * 60), 2)
# Calculate hours
hour <- (ride_length_ms / 60)
# Calculate minutes
min <- (hour - trunc(hour)) * 60
# Calculate seconds
sec <- (min - trunc(min)) * 60
# Convert into hour:minute:second format with format a leading zero (xx)
ride_length_var <- paste(sprintf("%02d", trunc(hour)),  sprintf("%02d", trunc(min)), sprintf("%02d", round(sec)), sep=":")
# Append the ride_length_var column
all_tripdata <- mutate(all_tripdata, ride_length = ride_length_var)

# Check variables
table(all_tripdata$member_casual)
table(all_tripdata$rideable_type)

# Create a column called "day_of_week" of each ride
day_of_week_var <- weekdays(as.Date(all_tripdata$started_at))
# Append the day_of_week_var column
all_tripdata <- mutate(all_tripdata, day_of_week = day_of_week_var)
all_tripdata <- mutate(all_tripdata, ride_length_difference = as.numeric(ride_length_diff))

# Descriptive analysis
summary(all_tripdata)

# Step 3: Doing analysis
aggregate(ride_length_difference ~ member_casual, data=all_tripdata,  mean)
aggregate(ride_length_difference ~ member_casual, data=all_tripdata,  max)
aggregate(ride_length_difference ~ member_casual + day_of_week, data=all_tripdata,  mean)

# Export CSV to store and do analysis on SQL (specify the path explicitly in case the file does not exist)
write.csv(all_tripdata, file="C:/Users/USER/Downloads/Downloads/tripdata_2022.csv")
