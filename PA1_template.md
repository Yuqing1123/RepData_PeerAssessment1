Here is my course project 1.
===============================

##Loading and preprocessing the data.


```r
setwd("D:/datascience/5")
data <- read.csv("activity.csv")
```

##What is mean total number of steps taken per day?

Calculate the total number of steps taken per day.


```r
TotalSteps <- aggregate(steps~date, data = data, sum, na.rm=TRUE)
```

Make a histogram of the total number of steps taken each day.


```r
hist(TotalSteps$steps,main="Total Number of Steps Taken Each Day",xlab="steps each day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Calculate and report the mean and median of the total number of steps taken per day.


```r
meanSteps <- mean(TotalSteps$steps)
medianSteps <- median(TotalSteps$steps)
```

The mean total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup> steps.
The median total number of steps taken per day is 10765 steps.

##What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
steps_interval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
plot(steps ~ interval, data = steps_interval, type = "l", xlab = "Time Intervals (5-minute)", ylab = "Mean number of steps taken (all Days)", main = "Average number of steps Taken at 5 minute Intervals",  col = "blue")
```

```
## Error in eval(m$data, eframe): object 'steps_interval' not found
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
MaxSteps_interval <- steps_interval[which.max(steps_interval$steps),"interval"]
```

```
## Error in eval(expr, envir, enclos): object 'steps_interval' not found
```

##Imputing missing values

Calculate and report the total number of missing values in the dataset.


```r
missing <- nrow(data[is.na(data),])
```

The total number of missing rows is`missing`.

Devise a strategy for filling in all of the missing values in the dataset.


```r
#This function returns the mean steps for a given interval.
getMeanStepsPerInterval <- function(interval){
    steps_interval[steps_interval$interval==interval,"steps"]
}
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data2 <- data
flag = 0
for (i in 1:nrow(data2)) {
    if (is.na(data2[i,"steps"])) {
        data2[i,"steps"] <- getMeanStepsPerInterval(data2[i,"interval"])
        flag = flag + 1
        }
}
```

```
## Error in getMeanStepsPerInterval(data2[i, "interval"]): object 'steps_interval' not found
```

Make a histogram of the total number of steps taken each day. 


```r
TotalSteps2 <- aggregate(steps~date, data = data2, sum)
hist(TotalSteps2$steps, xlab = "Total Number of Steps", main = "Histogram of Total Number of Steps taken each Day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

Calculate and report the mean and median total number of steps taken per day.


```r
meanSteps2 <- mean(TotalSteps2$steps)
medianSteps2 <- median(TotalSteps2$steps)
```

Mean total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>.
Median total number of steps taken per day is 10765.

Do these values differ from the estimates from the first part of the assignment?

The mean value is the same but the median value has changed.

What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean value is the same as the value before imputing missing data since the mean value has been used for that particular 5-min interval. The median value is different, since the median index is now being changed after imputing missing values.

##Are there differences in activity patterns between weekdays and weekends?


```r
data2$day <- ifelse(as.POSIXlt(as.Date(data2$date))$wday%%6 == 0, "weekend", "weekday")
data2$day <- factor(data2$day, levels = c("weekday", "weekend"))
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps.interval= aggregate(steps ~ interval + day, data2, mean)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, type = "l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'steps.interval' not found
```
