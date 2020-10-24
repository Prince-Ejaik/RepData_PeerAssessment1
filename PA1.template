---
title: "Reproducible Research Course: Peer Assessment Project 1"

author: "Anthony Efonemchi"

date: "23 10 2020"
output: html_document
---

# Background Information of the Assignment

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Data

The dataset for this assignment can be downloaded from the course web site: [Activity monitoring data](https://github.com/rdpeng/RepData_PeerAssessment1)

The variables included in this dataset are:

**steps:** Number of steps taking in a 5-minute interval (missing values are coded as NA)

**date:** The date on which the measurement was taken in YYYY-MM-DD format

**interval:** Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Load libraries
```{r warning = FALSE}
library(dplyr)
library(ggplot2)
# Change time locale for R if need be.
Sys.setlocale("LC_TIME", "English")
```

### Load and preprocess the data for the project.
```{r}
# Load core data.
activity <- read.csv("activity.csv")
```

```{r activity}
# Inspect the dataset for a quick overview.
summary(activity)
str(activity)
```

# **Questions**



## 1. What is the mean total number of steps taken per day?

### i. Total number of steps taken per day.
```{r}
# Deploy the aggregate function, remove NAs.
daily_Steps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
```

### ii. Construct the histogram of the total number of steps taken each day.
```{r}
# Plot the histogram with base plotting
hist(daily_Steps$steps, xlab = "Steps", main = "Total number of steps taken per day", col = "grey")
```

### iii. Mean and median of the total number of steps taken per day.
```{r}
# Mean
mean(daily_Steps$steps, na.rm=TRUE)
# Median
median(daily_Steps$steps, na.rm=TRUE)
```

## What is the average daily activity pattern?

### i. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).
```{r}
# Time series plot
activity_Steps_Average <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
#Plot
plot(activity_Steps_Average$interval, activity_Steps_Average$steps, type = "l", col = "green", xlab = "Intervals", ylab = "Steps", main = "Average steps per time interval")
```

### ii. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
# What is the highest steps value? (maximum of steps on one given interval)
highest_Steps <-max(activity_Steps_Average$steps)
# For which interval are the numbers of steps per interval at the highest?
highest_Interval <- activity_Steps_Average$interval[which(activity_Steps_Average$steps == highest_Steps)]
highest_Steps <- round(highest_Steps, digits = 2)
```

**The highest number of steps for a 5-minute interval is 206.17, which amounts to interval of 835.**

## Imputing missing values.

### i. Calculate total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs).
```{r}
sum(is.na(activity))
```

### ii. Devise a strategy for filling in all of the missing values in the dataset.
```{r}
# Filling in missing values with median of dataset. 
activity_Revised_Steps <- ifelse(is.na(activity$steps), round(activity_Steps_Average$steps[match(activity$interval, activity_Steps_Average$interval)],0), activity$steps)
```

### iii. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
# New dataset 
activity_New <- data.frame(steps=activity_Revised_Steps, interval=activity$interval, date=activity$date)
# See the first 6 values of the new dataset.
head(activity_New)
```

### iv. Make a histogram of the total number of steps taken each day? 
```{r}
steps_Daily_New <- aggregate(steps ~ date, data = activity_New, FUN = sum, na.rm = TRUE)
hist(steps_Daily_New$steps, xlab = "Steps", main = "Steps per day using new dataset", col = "red")
```

### v. Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
# Mean
mean(steps_Daily_New$steps)
# Median
median(steps_Daily_New$steps)
```

**In essence: Imputing missing values didn’t change the mean value in any way, whereas the median value experienced a negligable change. Both histograms display similar structure.**

### Are there differences in activity patterns between weekdays and weekends?
```{r}
# First of all create variable with date in correct format
activity_New$New_Date <- as.Date(activity_New$date, format = "%Y-%m-%d")
# Then, create a variable with weekday's name
activity_New$weekday <- weekdays(activity_New$New_Date)
# Finally, create a new variable indicating weekday or weekend
activity_New$Day_Type <- ifelse(activity_New$weekday=='Saturday' | activity_New$weekday=='Sunday', 'weekend','weekday')
# See the first 7 values
head(activity_New, 7)
```

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)?
```{r}
# Create a table with steps per time across weekdays or weekends.
steps_Interval_Day_Type <- aggregate(steps~interval+Day_Type,data=activity_New,FUN=mean,na.action=na.omit)
# Verify new dataframe. 
head(steps_Interval_Day_Type)
# Add descriptive variables.
names(steps_Interval_Day_Type) <- c("interval", "Day_Type", "activity_Steps_Average")
# Draw the ggplot.
plot <- ggplot(steps_Interval_Day_Type, aes(interval, activity_Steps_Average))
plot+geom_line(col="green")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Intervals")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(Day_Type ~ .)
```

**Conclusion: The plots appear similar in terms of activity patterns. However, there is a subtle difference with the weekday plot appearing to peak earlier than the one of the weekend.**
