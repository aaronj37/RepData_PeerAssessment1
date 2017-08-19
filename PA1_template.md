# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Load the data

```r
activity_data<-read.csv(unzip("activity.zip"))
```
Get the Total number of steps taken each day

```r
total_steps<-aggregate(steps~date,data=activity_data,sum,na.remove=TRUE)
```

## What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day

```r
hist(total_steps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
mean(total_steps$steps)
```

```
## [1] 10767.19
```

```r
median(total_steps$steps)
```

```
## [1] 10766
```



## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Process Data into Intervals

```r
interval_steps<-aggregate(steps~interval,data=activity_data,mean,na.rm=TRUE)
```
Plot Intervals

```r
plot(steps~interval,data=interval_steps,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
interval_steps[which.max(interval_steps$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset.

Create new data set and fill in missing values with the mean of five minute intervals


```r
new_activity_data<-activity_data

nas <- is.na(new_activity_data$steps)

interval_mean <- tapply(new_activity_data$steps,new_activity_data$interval, mean,na.rm=TRUE, simplify = TRUE)
new_activity_data$steps[nas] <- interval_mean[as.character(new_activity_data$interval[nas])]
```
Show that new data set has no missing values

```r
sum(is.na(new_activity_data$steps))
```

```
## [1] 0
```


Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
new_total_steps<-aggregate(steps~date,data=new_activity_data,sum)
hist(new_total_steps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
mean(new_total_steps$steps)
```

```
## [1] 10766.19
```

```r
median(new_total_steps$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean went down and the median went up

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
new_activity_data$day=ifelse(as.POSIXlt(as.Date(new_activity_data$date))$wday%%6==0,
                          "weekend","weekday")

new_activity_data$day=factor(new_activity_data$day,levels=c("weekday","weekend"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 



```r
new_interval_steps=aggregate(steps~interval+day,new_activity_data,mean)
library(lattice)
xyplot(steps~interval|factor(day),data=new_interval_steps,aspect=1/2,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

