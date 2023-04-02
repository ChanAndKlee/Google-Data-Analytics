# Case Study 1: How Does a Bike-Share Navigate Speedy Success

## Table of Contents
- [Case Study 1: How Does a Bike-Share Navigate Speedy Success](#case-study-1-how-does-a-bike-share-navigate-speedy-success)
  - [Table of Contents](#table-of-contents)
  - [Clear Statement of the Business Tasks](#clear-statement-of-the-business-tasks)
  - [Data Sources](#data-sources)
  - [Documentation of Cleaning or Manipulating Data](#documentation-of-cleaning-or-manipulating-data)
  - [Summary of your Analysis](#summary-of-your-analysis)
  - [Supporting Visualizations and Key Findings](#supporting-visualizations-and-key-findings)
    - [Summarize Key Findings](#summarize-key-findings)

---

## Clear Statement of the Business Tasks
```Cyclistic``` is a bike-share company located in Chicago, USA offering 5,824 bicycles in a network of 692 stations across Chicago. Cyclistic’s finance analysts have concluded that __annual members are much more profitable than casual riders__, therefore Moreno, the director of marketing strongly believes that maximizing the number of annual members will be key to future growth by converting casual riders into annual members rather than creating a marketing campaign for targeting all new customers.

In order to do that, there are 3 questions that we will need to figure out to better understand the proposed solution before taking action:  
1. How do annual and casual riders use Cyclistic bikes differently?  
2. Why would casual riders buy Cyclistic annual memberships?  
3. How can Cyclistic use digital media to influence casual riders to become members?  

## Data Sources
All of the related data sources are derived from the Google Data Analytics Course and can be found at: <a target="blank" href="https://divvy-tripdata.s3.amazonaws.com/index.html">LINK</a> in this analysis, we will be using the data sources between __January 2022 to December 2022__ to answer those business questions.  

The columns in this dataset include:
- ride_id
- rideable_type ("electric_bike", "classic_bike", "docked_bike")
- started_at (YYYY-MM-DD HH:MM:SS)
- ended_at (YYYY-MM-DD HH:MM:SS)
- start_station_name
- start_station_id 
- end_station_name
- end_station_id
- start_lat (latitude)
- start_lng (longitude)
- end_lat
- end_lng
- member_casual ("casual", "member")

```r
# Step 1: Setting up environment in R
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(dplyr)
library(readxl)
library(knitr)
library(ggplot2)

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

# Explore dataset (optional)
View(tripdata_2022_02)
colnames(tripdata_2022_01)
unique(tripdata_2022_02$start_station_name)
```

## Documentation of Cleaning or Manipulating Data
These are what I have done on cleaning steps:
- Explore the dataset, check the variable types, change if needed
- Combine all 12 months of tripdata into a single dataframe
```r
# Combine many lists into a single list using bind_rows()
all_tripdata <- bind_rows(tripdata_2022_01, tripdata_2022_02, tripdata_2022_03, tripdata_2022_04,
                          tripdata_2022_05, tripdata_2022_06, tripdata_2022_07, tripdata_2022_08,
                          tripdata_2022_09, tripdata_2022_10, tripdata_2022_11, tripdata_2022_12)
typeof(all_tripdata)
```

- Check the fixed-format variable (e.g., casual, member on member_casual column)
```r
# Check variables
table(all_tripdata$member_casual)
table(all_tripdata$rideable_type)
table(all_tripdata$day_of_week)
```

- Create a column ```ride_length``` to store the difference between end and start ride
```r
ride_length_diff <- difftime(all_tripdata$ended_at, all_tripdata$started_at)
ride_length_ms <- round((ride_length_diff / 3600 * 60), 2)
# Calculate hours
hour <- (ride_length_ms / 60)
# Calculate minutes
min <- (hour - trunc(hour)) * 60
# Calculate seconds
sec <- (min - trunc(min)) * 60
# Convert into hour:minute:second format with format a leading zero
ride_length_var <- paste(sprintf("%02d", trunc(hour)),  sprintf("%02d", trunc(min)), sprintf("%02d", round(sec)), sep=":")
# Append the ride_length_var column
all_tripdata <- mutate(all_tripdata, ride_length = ride_length_var)
# It cannot calculate the hms format directly, numeric is requiered for doing analysis
all_tripdata <- mutate(all_tripdata, ride_length_difference = as.numeric(ride_length_diff))
```

- Create a column ```day_of_week``` to store the weekday of each ride (in this case start and end ride is the same, so both works)
```r
# Create a column called "day_of_week" of each ride
day_of_week_var <- weekdays(as.Date(all_tripdata$started_at))
# Append the day_of_week_var column
all_tripdata <- mutate(all_tripdata, day_of_week = day_of_week_var)
all_tripdata <- mutate(all_tripdata, ride_length_difference = as.numeric(ride_length_diff))
```

- Check and remove NA records by using
```r
# Since we have many data, I will remove Rows with NA
all_tripdata <- na.omit(all_tripdata)
```

- Descriptive analysis (preview)
```r
# Descriptive analysis
summary(all_tripdata)
```

## Summary of your Analysis
```r
aggregate(ride_length_difference ~ member_casual, data=all_tripdata,  mean)
aggregate(ride_length_difference ~ member_casual, data=all_tripdata,  max)
aggregate(ride_length_difference ~ member_casual + day_of_week, data=all_tripdata,  mean)
```
- Export data in CSV file format
```r
# Export CSV to store and do analysis on SQL (specify the path explicitly in case the file does not exist)
write.csv(all_tripdata, file="C:/Users/USER/Downloads/Downloads/tripdata_2022.csv")
```

## Supporting Visualizations and Key Findings
__Graph 1__ shows the difference between "Casual and Member on Day of Week", it shows that most of riders prefer to have a ride on Saturday the most, and the number between casual and member riders are not quite different much.
![graph1](https://i.imgur.com/Vr3fGlh.png)  
__Graph 2__ shows the Casual Rider's Rideable type (types of bike that the casual riders use) It shows that most of casual riders prefer to have a ride with classic bike followed by electric bike.  
![graph2](https://imgur.com/8qc5kJz.png)  
__Graph 3__ shows the Member Rider's Rideable type (types of bike that the member riders use) It shows that most of member riders prefer to have a ride with classic bike followed by electric bike.  
![graph3](https://imgur.com/H0m7Jop.png)  
__Graph 4__ shows the correlation between month and amount of riders, most of riders are membership.
![graph4](https://imgur.com/H6XwBEH.png)  
__Graph 5 and 6__ shows the top 10 start and stop station used by the riders 
![graph5](https://imgur.com/i24VPUs.png)
![graph6](https://imgur.com/GSPsZb0.png)

### Summarize Key Findings
- It shows that most of riders (both casual and member) prefer to have a ride on __Saturday__ the most, and the least one is on __Monday__.
- Most riders tend to prefer using __Classic and Electronic bike__ respectively.
- In 2022, the highest month that received that has the most riders is between __June and July.__ (Therefore, the company may analyze further on what factor that has a correlation with this value)
- Top start and stop station is almost the same.