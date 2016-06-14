# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(dplyr)
library(chron)
library(ggplot2)
```


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url, "activity.zip")
unzip("activity.zip")

dat <- read.csv("activity.csv")
```



## What is mean total number of steps taken per day?

```r
totSteps <- tapply(dat$steps, dat$date, FUN = sum, na.rm = TRUE)

hist(totSteps
   , main = "Total Number of Steps Taken Each Day"
   , xlab = "Steps"
   )
```

![](PA1_template_files/figure-html/mean_steps-1.png)

```r
stepMean <- format(mean(totSteps, na.rm = TRUE), scientific = FALSE)
stepMid  <- format(median(totSteps, na.rm = TRUE), scientific = FALSE)
```

The mean of the total number of steps taken per day is 9354.23.
The median of the total number of steps taken per day is 10395.



## What is the average daily activity pattern?

```r
daily <- dat %>%
    na.omit() %>%
    group_by(interval) %>%
    summarise(steps = mean(steps))

plot(daily$interval
   , daily$steps
   , type = "l"
   , xlab = "Interval"
   , ylab = "Steps"
   , main = "Average Daily Activity Pattern"
   )
```

![](PA1_template_files/figure-html/daily_pattern-1.png)

```r
dailyMax <- daily %>%
    slice(which.max(steps))
```
   
The 5-minute interval on average that contains the maximum number of steps is 835.

    
    
## Imputing missing values
I chose to fill in all of the missing values in the dataset with the mean for that 5-minute interval.


```r
totMiss <- sum(is.na(dat$steps))

impDat <- dat %>%
    group_by(interval) %>%
    mutate(steps = replace(steps, is.na(steps), mean(steps, na.rm = TRUE)))
           
totSteps <- tapply(impDat$steps, impDat$date, FUN = sum, na.rm = TRUE)

hist(totSteps
   , main = "Total Number of Steps Taken Each Day"
   , xlab = "Steps"
   )
```

![](PA1_template_files/figure-html/imp_missing-1.png)

```r
stepMean <- format(mean(totSteps, na.rm = TRUE), scientific = FALSE)
stepMid  <- format(median(totSteps, na.rm = TRUE), scientific = FALSE)
```

The total number of missing values in the dataset (coded as NA) was 2304.
The mean of the total number of steps taken per day is 10766.19.
The median of the total number of steps taken per day is 10766.19.



## Are there differences in activity patterns between weekdays and weekends?

```r
impDat <- impDat %>%
    mutate(day = ifelse(is.weekend(as.Date(date)), "weekend", "weekday"))

weekly <- impDat %>%
    group_by(interval, day) %>%
    summarise(steps = mean(steps))

ggplot(weekly, aes(interval, steps)) +
    geom_line() +
    ggtitle("Weekday vs Weekend Daily Activity Patterns") +
    facet_grid(day ~ .) +
    xlab("Interval") +
    ylab("Steps")
```

![](PA1_template_files/figure-html/week_pattern-1.png)
