# Data analysis using uber data set

# import all the library
library(ggplot2) #visualization
library(ggthemes) #add-on with ggplot
library(lubridate) #date time
library(dplyr) #data manipulation
library(tidyr) #tidy data
library(DT) #table formatted result 
library(scales) #graphical scaling

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

# reading chunk of data
apr_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/April.csv")
may_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/May.csv")
jun_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/June.csv")
jul_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/July.csv")
aug_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/August.csv")
sep_data <- read.csv("C:/Users/npsan/OneDrive/Documents/Uber data/September.csv")

# Combine the data together 
data <- rbind(apr_data, may_data, jun_data, jul_data, aug_data, sep_data)
cat("The dimensions of the data are:",dim(data))
# Print the first 6 rows of the data
head(data)

# Convert to standard date and time format
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date.Time <- ymd_hms(data$Date.Time)
head(data)

# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))
head(data)

# Add Time variables as well 
data$second = factor(second(hms(data$Time)))
data$minute = factor(minute(hms(data$Time)))
data$hour = factor(hour(hms(data$Time)))
head(data)


# Data Visualization
# 1) Plot the trips by hours
hourly_data <- data %>% group_by(hour) %>% dplyr::summarize(Total = n())
# Plot the data by hour
ggplot(hourly_data, aes(hour, Trips)) + geom_bar(stat="identity", fill="red",) + ggtitle("Trips Every Hour") + scale_y_continuous(labels=comma)

# 2) Plot trips by hour and month
# Aggregate the data by month and hour
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())
# `summarise()` has grouped output by 'month'. You can override using the `.groups` argument
ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)

# 3) Plot trips during every day of the month
# Aggregate data by day of the month 
day_data <- data %>% group_by(day) %>% dplyr::summarize(Trips = n())
# Plot the data for the day
ggplot(day_data, aes(day, Trips)) + 
  geom_bar(stat = "identity", fill = "pink") +
  ggtitle("Trips Every Day ") + 
  scale_y_continuous(labels = comma)

# 4) Collect data by day of the week and month
day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
# `summarise()` has grouped output by 'dayofweek'. You can override using the `.groups` argument
# Plot the Collect data by day of the week and month
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

# 5) Number of Trips during months in a year
month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())
# Plot the Number of Trips place during months in a year
ggplot(month_data, aes(month, Total, fill = month)) + 
  geom_bar(stat = "Identity") + 
  ggtitle("Trips in a month") + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)


# 6) Plot the number of trips by Bases 
base_data <- data %>% group_by(Base) %>% dplyr::summarize(Trips = n())
datatable(base_data)
# Plot the number of trips by Bases 
ggplot(data, aes(Base))+geom_bar(fill = "darkblue")+scale_y_continuous(labels=comma)+
ggtitle("Trips by Bases")

#The End... 


