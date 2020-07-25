---
title: "Reproducible Research Project 1"
author: "Mark Rayfield"
date: "7/24/2020"
output: 
  html_document: 
    keep_md: yes
---



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November 2012 (61 days) and includes the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

* Dataset : [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip") [52k]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
* **date**: The date on which the measurement was taken in YYYY-MM-DD format
* **interval**: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data

Load packages to be used in analysis. Download, unzip and read the csv file, specify "date" in Date format. 


```r
library("data.table")
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
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
library("ggplot2")
fileURLZip <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURLZip, destfile= "temp.zip", method = "curl")
unzip("temp.zip")
activity <- data.table::fread(input = "activity.csv")
activity[,date:=as.Date(date,"%Y-%m-%d")]
```

## What is mean total number of steps taken per day?

Missing steps values in the dataset are ignored for this analysis. Using dply functions group by day and sum steps per day. This results in 61 daily observations with 8 days having NA values. A histogram of the valid days is then generated and mean and median number of dialy steps is reported.  


```r
activityDay <- activity %>% group_by(date) %>% summarise(Dailysteps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(activityDay, aes(x = Dailysteps)) +
    geom_histogram(fill = "red", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](Instructions_fig/fig-mean daily steps-1.png)<!-- -->

```r
summarise(activityDay, mean_daily=mean(Dailysteps, na.rm=TRUE), median_daily = median(Dailysteps, na.rm=TRUE))
```

```
## # A tibble: 1 x 2
##   mean_daily median_daily
##        <dbl>        <int>
## 1     10766.        10765
```

## What is the average daily activity pattern?

A time series line plot is generated of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). Through manipulation the interval data has been plotted using a POSIXct date format though this was not required. 


```r
activityInt <- activity %>% group_by(interval) %>% summarise(IntvAvg=mean(steps, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
start <- as.POSIXct("2012-10-01 00:00", "%Y-%m-%d %H:%M", tz = "")
end <- as.POSIXct("2012-10-01 23:55", "%Y-%m-%d %H:%M", tz = "")
x <- seq(start, end, by=300)
activityInt <-mutate(activityInt, Time_of_Day = x)
with(activityInt, (plot(Time_of_Day, IntvAvg, type = "l" )))
```

![](Instructions_fig/fig-daily activity pattern-1.png)<!-- -->

```
## NULL
```

*Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?* The code below reports the time of day in Hr:Min

```r
activityInt <-mutate(activityInt, TOD = paste(hour(Time_of_Day),minute(Time_of_Day), sep =":"))
MaxSteps <- activityInt$TOD[which.max(activityInt$IntvAvg)]
print (paste("5 min interval with maximum steps across all days starts at ", MaxSteps))
```

```
## [1] "5 min interval with maximum steps across all days starts at  8:35"
```


## Imputing missing values

Assignment promts and questions are showin in *italics*.
*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. Calculate and report the total number of missing "steps" values in the dataset (i.e. the total number of rows with NA's)*


```r
nrow(activity[is.na(steps),])
```

```
## [1] 2304
```

*Replace missing "steps" values and create a tidy dataframe and set.* Done by replacing NA interval data using the average 5 minute mean from previous question as a time dependent projection of the outcome. A tidy dataframe "activityTidy" is generarted which matches the format of the original, this could be outputted as an alternative csv file with the fwrite statement.  


```r
vect1 <- activityInt$IntvAvg
vect2 <- rep(vect1, 61) ## 17,568 long vector 288*61
activityTemp <- cbind(activity, vect2)
activityTemp$steps[which(is.na(activity))] <- activityTemp$vect2
```

```
## Warning in activityTemp$steps[which(is.na(activity))] <- activityTemp$vect2:
## number of items to replace is not a multiple of replacement length
```

```r
activityTidy <-activityTemp[,-4]
## data.table::fwrite(x = activityTidy, file = "data/tidyData.csv", quote = FALSE)
```

*Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.* 


```r
activityTidyDay <- activityTidy %>% group_by(date) %>% summarise(Dailysteps=sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
ggplot(activityTidyDay, aes(x = Dailysteps)) +
    geom_histogram(fill = "red", binwidth = 500) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](Instructions_fig/fig-mean daily steps imputed-1.png)<!-- -->

```r
summarise(activityTidyDay, mean_daily=mean(Dailysteps), median_daily = median(Dailysteps))
```

```
## # A tibble: 1 x 2
##   mean_daily median_daily
##        <dbl>        <dbl>
## 1     10766.       10766.
```

*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?* 
Imputing the 2,304 missing NA values with the mean interval values increases the number of **average** daily days. Global median and mean are unchnaged as intended, it is observed that 63% of 5 min interval step values of zero remain and dominate the tidy data set distribution.

```r
zero <- activityTidy$steps == 0
sum(zero)
```

```
## [1] 11166
```

## Are there differences in activity patterns between weekdays and weekends?

*For this part the weekdays()function may be of some help here. Use the dataset with the filled-in missing values for this part.*

*Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.*



```r
activityTidy[, "DOW":= weekdays(x = date)]
activityTidy[, "weekday_weekend":= "weekday"]
activityTidy[grepl(pattern = "Saturday|Sunday", x = DOW), "weekday_weekend"] <- "weekend"
activityTidy$weekday_weekend <- as.factor(activityTidy$weekday_weekend)
table(activityTidy$weekday_weekend)
```

```
## 
## weekday weekend 
##   12960    4608
```

*Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*


```r
activityTidyInt <- activityTidy %>% group_by(interval,weekday_weekend) %>% summarise(IntvAvg=mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
ggplot(activityTidyInt , aes(x = interval , y = IntvAvg, color=weekday_weekend)) +
    geom_line() + 
    labs(title = "Average daily interval steps", x = "5 min intervals", y = "No. of Steps") +
    facet_wrap(~weekday_weekend , ncol = 1)
```

![](Instructions_fig/fig-panel plot days of week-1.png)<!-- -->

