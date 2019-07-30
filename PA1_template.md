---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, create an folder to download the data and download it.



```r
if(!file.exists("./data")) {
        dir.create("./data")
}

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "./data/file.zip")
```

Unzip, read and transform the data with dplyr and lubridate for analysis.

```r
unzip("./data/file.zip", exdir = "./data")
activity <- read.csv("./data/activity.csv")
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity$date <- ymd(activity$date)
activity <- tbl_df(activity)
```


## What is mean total number of steps taken per day?
Calculate the total number of steps taken per day.  

```r
total.steps <- activity %>% group_by(date) %>% summarize(total = sum(steps))
```

Make a histogram of steps taken per day.  

```r
library(ggplot2)
ggplot(data = total.steps) + geom_histogram(aes(total), bins = 9)+labs(x = "Total number of steps per day", y = "Frequency", title = "Histogram - number of steps per day") + theme(plot.title = element_text(hjust = 0.5))
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/histogram.of.steps.taken-1.png)<!-- -->

Calculate and report the mean and median of the total number of steps taken per day.  

```r
steps.mean <- mean(total.steps$total, na.rm = TRUE)
steps.median <- median(total.steps$total, na.rm = TRUE)
```
The mean of total number of steps is 10766.19.  
The median of total number of steps is 10765.

## What is the average daily activity pattern?
Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).  

```r
average.steps <- activity %>% group_by(interval) %>% summarize(av.steps = mean(steps, na.rm = TRUE))
ggplot(data = average.steps, aes(x = interval, y = av.steps)) + geom_line() + labs(x = "Interval", y = "Average number of steps taken", title = "Average daily activity pattern") + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/time.series.average.steps.interval-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
maximum.number.of.steps.interval <- average.steps[average.steps$av.steps == max(average.steps$av.steps), 1]
```

The maximum number of steps occurs at 835 minute interval.

## Imputing missing values

```r
total.number.of.missing.values <- sum(is.na(activity$steps))
```

The total number of missing values is 2304.  
For filling in all the missing values in the dataset, the mean for that 5-minute interval was used, as below.  

```r
na.data <- activity[is.na(activity$steps), ]
not.na.data <- activity[!is.na(activity$steps), ]
N <- dim(na.data)[1]
for(i in 1:N){
        int <- as.numeric(na.data[i,"interval"])
        step_sub <- as.numeric(average.steps[average.steps$interval==int, "av.steps"])
        na.data[i ,"steps"] <- step_sub
}

filled <- rbind(na.data, not.na.data)
```

The histogram of the total number of steps taken each day is below.  

```r
total.filled.steps <- filled %>% group_by(date) %>% summarize(total = sum(steps))
ggplot(data = total.filled.steps) + geom_histogram(aes(total), bins = 9)+labs(x = "Total number of steps per day", y = "Frequency", title = "Histogram - number of steps per day") + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/histogram.filled.steps-1.png)<!-- -->

```r
filled.steps.mean <- mean(total.filled.steps$total)
filled.steps.median <- median(total.filled.steps$total)
```

The mean of the filled dataset is 10766.19 (opposed to 10766.19 in the original dataset).  
The median of the filled dataset is 10766.19 (opposed to 10765 in the original dataset).  
The values are similar to the estimated in the first part of the assignment. So, it seems that there is no impact of the missing values in the dataset.

## Are there differences in activity patterns between weekdays and weekends?
First, two variables were created indicating the weekday and classifying if it is a weekday or weekend day. I'm in Brazil, so the weekdays are shown in portuguese.  


```r
weekday <- filled %>% mutate(day = weekdays(date)) %>% filter(day != "sábado" & day != "domingo") %>% mutate(day2 = "weekday")
weekend <- filled %>% mutate(day = weekdays(date)) %>% filter(day == "sábado" | day == "domingo") %>% mutate(day2 = "weekend")
```

The panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, average across all week days or weekend days (y-axis) is below.   

```r
activity_day <- rbind(weekday, weekend) %>% group_by(interval, day2) %>% summarize(av.steps = mean(steps, na.rm = TRUE))
ggplot(data = activity_day, aes(x = interval, y = av.steps, group = day2)) + geom_line() + facet_grid(day2 ~ .) + labs(x = "Interval", y = "Average number of steps taken", title = "Average daily activity pattern per day") + theme(plot.title = element_text(hjust = 0.5))
```

![](PA1_template_files/figure-html/weekdays.compare-1.png)<!-- -->

Yes, are differences in activity pattenrs between weekdays and weekends.

