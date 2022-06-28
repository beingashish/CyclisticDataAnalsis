#Step 1 Load the packages
library("tidyverse")
library("lubridate")
library("ggplot2")

#Step 2 Set the current working directory
setwd("D:/GDAC/Cyclistic Project/12_Months Data")

#Step 3: Merge the csv files

New_Data <- ldply(list.files(), read.csv, header=TRUE)

#Step 4: Drop unnecessary columns
New_Data<-select(New_Data, -5:-12)
New_Data<-select(New_Data, -6)

 #Step 4: Examine the data
View(New_Data)
str(New_Data)
summary(New_Data)

#Step 5: Calculate total duration of ride time
#"New_Data1" is the back up of original data "New_Data"
New_Data1 <- New_Data %>% mutate(Total_Ride_Time = as.Date(ended_at) - as.Date(started_at)) %>% 
  mutate(Weekday = weekdays(as.Date(New_Data$started_at)))

View(New_Data1)

#Calculating the total ride length and storing it in a separate object called "Total Ride Time"

New_Data1$Total_Ride_Time<-as.numeric(difftime(New_Data1$ended_at, New_Data1$started_at, units = "mins"))

#Step 6: Removing ride time with values less than 0 minutes and more than 1440 minutes
New_Data1<-subset(New_Data1, !Total_Ride_Time<0)
New_Data1<-subset(New_Data1, !Total_Ride_Time>1440)

#Analyze the data
#Check for outlier
mean(New_Data1$Total_Ride_Time)
max(New_Data1$Total_Ride_Time)
min(New_Data1$Total_Ride_Time)
median(New_Data1$Total_Ride_Time)

#Analyze each user type
aggregate(Total_Ride_Time~member_casual, New_Data1, mean)
aggregate(Total_Ride_Time~member_casual, New_Data1, median)

#Check for rider data by type and day of the week
New_Data1 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(Total_Ride_Time)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)

#Visualize the data
#Number of rides by rider types and average duration
New_Data1%>%
  mutate(weekday1=wday(started_at))%>%
  group_by(member_casual, weekday1)%>%
  summarise(number_of_rides=n(), average_duration=mean(Total_Ride_Time))%>%
  arrange(member_casual, weekday1)%>%
  ggplot(aes(x=weekday1, y=number_of_rides, fill=member_casual))%>%
  geom_col()


           
  

  