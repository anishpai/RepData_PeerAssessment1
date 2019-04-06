---
title: "Course Project 1"
author: "Anish Suresh Pai"
date: "6 April 2019"
output: html_document

---
## 1. Code for reading in the dataset and/or processing the data
```{r, echo=TRUE}
setwd("C:\\Users\\pais\\Documents\\R")
activityDT <- data.table::fread(input = "activity.csv")
```
#Data Summary
```{r,echo=TRUE}
head(activityDT)

names(activityDT)

str(activityDT)


sum(is.na(activityDT$steps))/dim(activityDT)[[1]]
#Converting into date format using lubridate
library(lubridate)
activityDT$date<-ymd(activityDT$date)
weekday<- weekdays(activityDT$date)
activityDT<- cbind(activityDT,weekday)

summary(activityDT)
```


## 2. Histogram of the total number of steps taken each day 

```{r,echo=TRUE}
totalstp<- with(activityDT, aggregate(steps, by= list(date),FUN=sum,na.rm=TRUE))
names(totalstp)<- c("date", "steps")
hist(totalstp$steps, main=" Total steps taken in per day", xlab="Total Steps", ylab="Number of Days", col="orange", ylim= c(0,20), breaks=seq(0,25000,by=2500))
```
## 3. Mean and median number of steps taken each day

1. Calculate the total number of steps taken per day

```{r, echo=TRUE}
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = TRUE)), .SDcols = c("steps"), by = .(date)] 
head(Total_Steps, 10)
```
2. Calculate and report the mean and median of the total number of steps taken per day
```{r, echo=TRUE}
Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

## 4. Time series plot of the average number of steps taken

```{r, echo=TRUE}
library(ggplot2)
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="blue", size=0.5) + labs(title= "   Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
IntervalDT[steps == max(steps), .(max_interval = interval)]
```


## 6. Code to describe and show a strategy for imputing missing data

1. Calculating the total number of missing values in the dataset???s)

```{r, echo=TRUE}
activityDT[is.na(steps), .N ]

```

2. Devise a strategy for filling in all of the missing values in the dataset is the mean for that 5-minute interval

```{r, echo=TRUE}
# Filling in missing values with median of dataset. 
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r, echo=TRUE}
data.table::fwrite(x = activityDT, file = "newdata.csv", quote = FALSE)
```

## 7. Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo=TRUE}
# total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

Type of Estimate | Mean_Steps | Median_Steps
--- | --- | ---
First Part (with na) | 10765 | 10765
Second Part (fillin in na with median) | 9354.23 | 10395

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r, echo=TRUE}

activityDT <- data.table::fread(input = "activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)
```

2. Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r, echo=TRUE}
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Day type", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```
##All the code to reproduce the results obtained
#a. Code for reading in data and reproducing histogram of the total number of steps taken each day (excluding incomplete cases/NAs).
```{r, echo=TRUE}
setwd("C:\\Users\\pais\\Documents\\R")
activityDT <- data.table::fread(input = "activity.csv")

head(activityDT)

names(activityDT)

str(activityDT)


sum(is.na(activityDT$steps))/dim(activityDT)[[1]]
#Converting into date format using lubridate
library(lubridate)
activityDT$date<-ymd(activityDT$date)
weekday<- weekdays(activityDT$date)
activityDT<- cbind(activityDT,weekday)

summary(activityDT)


totalstp<- with(activityDT, aggregate(steps, by= list(date),FUN=sum,na.rm=TRUE))
names(totalstp)<- c("date", "steps")
hist(totalstp$steps, main=" Total steps taken in per day", xlab="Total Steps", ylab="Number of Days", col="orange", ylim= c(0,20), breaks=seq(0,25000,by=2500))
```
#b. Code for reproducing mean and median number of steps taken each day (excluding incomplete cases/NAs). 

```{r,echo=TRUE}
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = TRUE)), .SDcols = c("steps"), by = .(date)] 
head(Total_Steps, 10)

Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

#c. Code for reproducing time series plot of the average number of steps taken (excluding incomplete cases/NAs).

```{r}
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="blue", size=0.5) + labs(title= "   Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```


#d. Code for reproducing The 5-minute interval that contains the maximum number of steps.
```{r,echo=TRUE}
IntervalDT[steps == max(steps), .(max_interval = interval)]

activityDT[is.na(steps), .N ]

nrow(activityDT[is.na(steps),])

# Filling in missing values with median of dataset. 
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]

data.table::fwrite(x = activityDT, file = "newdata.csv", quote = FALSE)

# total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```
# e. Code for reproducing panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```{r,echo=TRUE}
 
activityDT <- data.table::fread(input = "activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Day type", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```



