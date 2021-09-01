library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(sqldf)
install.packages("sqldf")
getwd()

df1 <- read.csv("~/CaseStudy1/202004-divvy-tripdata/202004-divvy-tripdata.csv")
df2 <- read.csv("~/CaseStudy1/202005-divvy-tripdata/202005-divvy-tripdata.csv")
df3 <- read.csv("~/CaseStudy1/202006-divvy-tripdata/202006-divvy-tripdata.csv")
df4 <- read.csv("~/CaseStudy1/202007-divvy-tripdata/202007-divvy-tripdata.csv")
df5 <- read.csv("~/CaseStudy1/202008-divvy-tripdata/202008-divvy-tripdata.csv")
df6 <- read.csv("~/CaseStudy1/202009-divvy-tripdata/202009-divvy-tripdata.csv")
df7 <- read.csv("~/CaseStudy1/202010-divvy-tripdata/202010-divvy-tripdata.csv")
df8 <- read.csv("~/CaseStudy1/202011-divvy-tripdata/202011-divvy-tripdata.csv")
df9 <- read.csv("~/CaseStudy1/202012-divvy-tripdata/202012-divvy-tripdata.csv")
df10 <- read.csv("~/CaseStudy1/202101-divvy-tripdata/202101-divvy-tripdata.csv")
df11 <- read.csv("~/CaseStudy1/202102-divvy-tripdata/202102-divvy-tripdata.csv")
df12 <- read.csv("~/CaseStudy1/202103-divvy-tripdata/202103-divvy-tripdata.csv")

# combine 12 dataset to 1
bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
dim(bike_rides)

# changing the started_at and ended_at to date format
bike_rides$started_at <-lubridate::ymd_hms(bike_rides$started_at)
bike_rides$ended_at <-lubridate::ymd_hms(bike_rides$ended_at)

bike_rides_v2 <- bike_rides

# filter out unnecessary variable
bike_rides<- bike_rides %>% 
  select(c(ride_id, rideable_type,started_at, ended_at, start_station_name, start_station_id, end_station_name,
         end_station_id, member_casual,))

# find duration of ride
bike_rides_v2$duration_in_s <- bike_rides_v2$ended_at-bike_rides_v2$started_at

table(bike_rides$member_casual)

# convert duration to numeric
is.factor(bike_rides_v2$duration_in_s)
bike_rides_v2$duration_in_s <- as.numeric(as.character(bike_rides_v2$duration_in_s))
is.numeric(bike_rides_v2$duration_in_s)

# remove trip from HQ QR and duration that is less than 0
bike_rides_v2 <- bike_rides_v2[!(bike_rides_v2$start_station_name == "HQ QR" | bike_rides_v2$duration_in_s<0),]

# conducting analysis
mean(bike_rides_v2$duration_in_s)
median(bike_rides_v2$duration_in_s)
max(bike_rides_v2$duration_in_s)
min(bike_rides_v2$duration_in_s)
summary(bike_rides_v2$duration_in_s)

aggregate(duration_in_s~member_casual,bike_rides_v2,mean)
aggregate(duration_in_s~member_casual,bike_rides_v2,median)
aggregate(duration_in_s~member_casual,bike_rides_v2,max)
aggregate(duration_in_s~member_casual,bike_rides_v2,min)

# create a day_of_week for, month, hour of the trip
bike_rides_v2$day_of_week <- format(as.Date(bike_rides_v2$started_at), "%A")
bike_rides_v2$month <- format(as.Date(bike_rides_v2$started_at), "%m")
bike_rides_v2$hour_of_trip <-hour(bike_rides_v2$started_at)

str(bike_rides_v2)

aggregate(bike_rides_v2$duration_in_s~bike_rides_v2$member_casual + bike_rides_v2$day_of_week,FUN = mean)

bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, 
                                     levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(duration_in_s)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Number of rides throughout the week")

bike_rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(duration_in_s)) %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title = "Average duration of ride throughout the week")
  
bike_rides_v2 %>% 
  group_by(member_casual, hour_of_trip) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual) %>%
  ggplot(aes(hour_of_trip, number_of_rides, color = member_casual))+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Hourly Usage")

bike_rides_v2 %>% 
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual) %>%
  ggplot(aes(month, number_of_rides, group = member_casual, color = member_casual))+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Monthly Usage")

bike_rides_v2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarise(number_of_rides = n()) %>%
  arrange(member_casual) %>%
  ggplot(aes(day_of_week, number_of_rides, group = member_casual, color = member_casual))+
  geom_line(size = 1)+
  theme_minimal()+
  labs(title = "Daily Usage")

tail(bike_rides_v2$start_station_name)

# find the top 10 most popular start station
bike_rides_v2 %>%
  filter(start_station_name != "" ) %>% 
  group_by(member_casual, start_station_name) %>% 
  summarise(n=n()) %>%
  arrange(desc(n))

bike_rides_v2 %>%
  filter(start_station_name != "") %>% 
  group_by(start_station_name) %>%
  count(member_casual, sort = (decreasing = T)) %>%
  subset(member_casual == "member") %>% 
  top_n(10)

bike_rides_v2 %>%
  filter(start_station_name != "") %>% 
  group_by(start_station_name) %>%
  count(member_casual, sort = (decreasing = T)) %>%
  subset(member_casual == "casual") %>% 
  top_n(10)
  
bike_rides_v2 %>% 
  summarise(number_of_rides = n()) %>%
  ggplot(aes(member_casual, number_of_rides, fill = member_casual))+
  geom_bar(position = "fill",
           alpha = 0.5)

bike_rides_v2 %>% 
  ggplot(aes(x = member_casual))+
  geom_bar()+
  labs(title = "Number of Member VS Casual")

              