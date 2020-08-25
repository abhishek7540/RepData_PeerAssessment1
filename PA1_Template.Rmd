---
title: "Reproducible Research: Week_2_Course_Project_1"
author: "Abhishek_Mukherjee"
date: "19/08/2020"
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
```




## **Loading and processing the data**

```{r load}
#Extracting and reading output as .csv
unzip(zipfile = "./activity.zip")
activity <- read.csv("./activity.csv", na.strings = "NA")

#changing date from character to date format
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```




## **What is mean total number of steps taken per day?**

```{r steps}
#Calculating total steps taken each day
stepsperday <- aggregate(steps ~ date, activity, FUN = sum)

png("Total Steps by each day.png")
qplot(x = date, y = steps, data = stepsperday) +  
      geom_bar(stat = "identity") + xlab("Date") + ylab("Total Steps") + 
      ggtitle("Total Steps by each day")
dev.off()

#Calculating mean and median steps taken per day
meanstepsperday <- mean(stepsperday$steps, na.rm = TRUE)
medianstepsperday <- median(stepsperday$steps, na.rm = TRUE)
```

### The mean total number of steps taken per day is `r meanstepsperday`, and the median is `r medianstepsperday`.




## **What is the average daily activity pattern?**

```{r activitypattern}
#Creating data set for plotting
dailyactivitypat <- aggregate(formula = steps ~ date + interval, data = activity, FUN = mean)

png("Time Series plot of the average number of steps taken.png")
qplot(x = interval, y = steps, data = dailyactivitypat) + geom_bar(stat = "identity") + 
      xlab("Interval") + ylab("Steps") + ggtitle("Time Series plot of the average number of steps taken")
dev.off()

#Calculating maximum steps
stepsperinterval <- aggregate(formula = steps ~ interval, data = dailyactivitypat, FUN = sum)
maxstepsinterval <- which.max(stepsperinterval[, 2])
```

### Interval `r maxstepsinterval` on average across all the days in the dataset, contains the maximum number of steps.




## **Imputing missing values**

```{r missingvalues}
#Calculating total number of missing values
totalMVs <- sum(is.na(activity))

#Replacing missing values with the mean and creating a new data set
newactivitydata <- activity
newactivitydata$steps <- ifelse(test = is.na(newactivitydata$steps), yes = mean(newactivitydata$steps, na.rm = TRUE), no = newactivitydata$steps)

#Plotting histogram with new activity data
newstepsperday <- aggregate(steps ~ date, newactivitydata, FUN = sum)

png("Total Steps by each day with imputed missing values.png")
ggplot(data = newstepsperday, aes(x = date, y = steps)) +  
      geom_bar(stat = "identity") + xlab("Date") + ylab("Total Steps") + 
      ggtitle("Total Steps by each day with imputed missing values")
dev.off()

#Calculating mean and median steps taken per day with new activity data
newmeanstepsperday <- mean(newstepsperday$steps, na.rm = TRUE)
newmedianstepsperday <- median(newstepsperday$steps, na.rm = TRUE)
```

### There are `r totalMVs` missing values classified as "NA" in the activity raw data.

### The mean total number of steps taken per day in the new activity data set is `r newmeanstepsperday`, and the median is `r newmedianstepsperday`, which is the same as the previously calculated mean and median. This is because we have used the mean value to replace the missing values in the new activity data.




## **Are there differences in activity patterns between weekdays and weekends?**

```{r weekday_weekend_activity_pattern}
#Creating a new factor variable to identify weekdays and weekends
weekdaysvector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
newactivitydata$day <- factor((weekdays(newactivitydata$date) %in% weekdaysvector), levels = c(FALSE, TRUE), labels = c("weekend", "weekday"))

#Creating data set for plotting
newdailyactivitypat <- aggregate(formula = steps ~ date + interval + day, data = newactivitydata, FUN = mean)

#Plotting data for weekends and weekdays
png("Mean Steps across intervals by weekends and weekdays.png")
qplot(x = interval, y = steps, data = newdailyactivitypat, facets = "day") + geom_line() + xlab("Intervals") + ylab("Mean Steps") + ggtitle("Mean Steps across intervals by weekends and weekdays")
dev.off()
```


