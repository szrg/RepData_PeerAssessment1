# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
setClass('activity_date')
setAs("character", "activity_date", function(from) as.Date(from, "%Y-%m-%d"))
data <- read.table(unz('activity.zip', 'activity.csv'), header=TRUE, sep=",", 
                   colClasses=c('numeric', 'activity_date', 'numeric'))
```


Calculate the total number of steps taken per day.


```r
total_steps_by_date <- aggregate(data$steps, by=list(data$date), FUN=sum)
colnames(total_steps_by_date) <- c('date', 'total')
hist(total_steps_by_date$total, main='Histogram of total number of steps taken each day', xlab=NULL)
total_steps_mean <- mean(total_steps_by_date$total, na.rm = TRUE)
total_steps_median <- median(total_steps_by_date$total, na.rm = TRUE)
abline(v=total_steps_mean, col='red')
abline(v=total_steps_median, col='blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Calculate the mean and median of the total number of steps taken per day.


```r
total_steps_mean
```

```
## [1] 10766.19
```

```r
total_steps_median
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Create time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
mean_steps_by_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
colnames(mean_steps_by_interval) <- c('interval', 'mean')
mean_steps_by_interval$time <- formatC(mean_steps_by_interval$interval, width=4, format='d', flag='0')
mean_steps_by_interval$time <- sub('([[:digit:]]{2,2})$', ':\\1', mean_steps_by_interval$time)
mean_steps_by_interval$time <- as.POSIXct(mean_steps_by_interval$time, format='%H:%M')
plot(mean_steps_by_interval$time, mean_steps_by_interval$mean, type='l', xlab='time', ylab='mean')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Find 5-minute interval containing the maximum number of steps on average across all the days.


```r
mean_steps_by_interval[max(mean_steps_by_interval$mean),'interval']
```

```
## [1] 1705
```

## Imputing missing values

Calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs).


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Fill in missing values. Replace missing value with particular mean computed for the given interval across all the days.


```r
data2 <- merge(x=data, y=mean_steps_by_interval, by='interval')
data2 <- data2[order(data2$date, data2$interval),]
data2[is.na(data2$steps),]$steps = data2[is.na(data2$steps),]$mean
```

Calculate the total number of steps taken per day.


```r
total_steps_by_date2 <- aggregate(data2$steps, by=list(data2$date), FUN=sum)
colnames(total_steps_by_date2) <- c('date', 'total')
hist(total_steps_by_date2$total, main='Histogram of total number of steps taken each day', xlab='Number of steps each day')
total_steps_mean2 <- mean(total_steps_by_date2$total)
total_steps_median2 <- median(total_steps_by_date2$total)
abline(v=total_steps_mean2, col='red')
abline(v=total_steps_median2, col='blue')
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

Calculate the mean and median of the total number of steps taken per day.


```r
total_steps_mean2
```

```
## [1] 10766.19
```

```r
total_steps_median2
```

```
## [1] 10766.19
```

As we can see, the mean and the median only slightly differ from the estimates from the first part of 
the assignment. In this particular case, the impact of imputing missing data is small and in my
opinion could be avoided.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data2$wd <- as.factor(weekdays(data2$date))
data2$day_type <- as.factor(ifelse(data2$wd %in% c("Saturday", "Sunday"), "weekend", "workday"))
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
data_workday <- subset(data2, day_type == 'workday')
mean_steps_by_interval_workday <- aggregate(data_workday$steps, by=list(data_workday$interval), FUN=mean)
colnames(mean_steps_by_interval_workday) <- c('interval', 'mean')
mean_steps_by_interval_workday$time <- formatC(mean_steps_by_interval_workday$interval, width=4, format='d', flag='0')
mean_steps_by_interval_workday$time <- sub('([[:digit:]]{2,2})$', ':\\1', mean_steps_by_interval_workday$time)
mean_steps_by_interval_workday$time <- as.POSIXct(mean_steps_by_interval_workday$time, format='%H:%M')

data_weekend<- subset(data2, day_type == 'weekend')
mean_steps_by_interval_weekend <- aggregate(data_weekend$steps, by=list(data_weekend$interval), FUN=mean)
colnames(mean_steps_by_interval_weekend) <- c('interval', 'mean')
mean_steps_by_interval_weekend$time <- formatC(mean_steps_by_interval_weekend$interval, width=4, format='d', flag='0')
mean_steps_by_interval_weekend$time <- sub('([[:digit:]]{2,2})$', ':\\1', mean_steps_by_interval_weekend$time)
mean_steps_by_interval_weekend$time <- as.POSIXct(mean_steps_by_interval_weekend$time, format='%H:%M')

par(mfrow=c(2,1))
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
plot(mean_steps_by_interval_workday$time, mean_steps_by_interval_workday$mean, type='l', ylim=c(0,250), xlab='time', ylab='mean', main='Workdays')
plot(mean_steps_by_interval_weekend$time, mean_steps_by_interval_weekend$mean, type='l', ylim=c(0,250), xlab='time', ylab='mean', main='Weekends')
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 