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
str(New_Data1)

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

#Visualizing the number of rides by rider types against day of the week
New_Data1%>%
  mutate(weekday1=wday(started_at, label = TRUE))%>%
  group_by(member_casual, weekday1)%>%
  summarise(number_of_rides=n(), average_duration=mean(Total_Ride_Time))%>%
  arrange(member_casual, weekday1)%>%
  ggplot(aes(x=weekday1, y=number_of_rides, fill=member_casual))+
  geom_col(position = "Dodge")+
  labs(title="Number of riders Vs Weekday")+
  xlab("Days of Week")+
  ylab("Number of Rides")

#Visualizing average duration by day of the week
New_Data1%>%
  mutate(weekday1=wday(started_at, label = TRUE))%>%
  group_by(member_casual, weekday1)%>%
  summarise(number_of_rides=n(), average_duration=mean(Total_Ride_Time))%>%
  arrange(member_casual, weekday1)%>%
  ggplot(aes(x=weekday1, y=average_duration, fill=member_casual))+
  geom_col(position = "Dodge")+
  labs(title="Number of riders Vs Weekday")+
  xlab("Days of Week")+
  ylab("Average Ride Length")+
  theme(panel.background = element_blank())+
  theme(panel.grid.major = element_line(color = "grey"))
  

#Visualize rides by month
New_Data1 %>%  
  mutate(Month1=format(as.Date(started_at,format = "%Y-%m-%d"), "%m"))%>%
  group_by(member_casual, Month1 ) %>%  
  summarise(number_of_rides = n(),average_duration = mean(Total_Ride_Time)) %>% 
  arrange(member_casual) %>% 
  ggplot(aes(x=Month1, y=number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) +
    theme(axis.text.x = element_text(angle = 45))

#Looking at breakdown of bike types rented
New_Data1 %>%    
  ggplot(aes(x = rideable_type, fill = member_casual)) + geom_bar(position = "dodge") + 
  labs(x= 'Bike Type', y='Number of Rentals', title='Membership wise bike usage', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mil", "1.5Mil"))


           
  

  