
# "Activity Monitoring Report"


### Open the data set 


```r
options(scipen = 1, digits = 2)
setwd("/Users/kshitij/Documents/Data Science/Reproducible Research")
activity <- read.csv ("activity.csv", header = TRUE, sep = ",")
```

### What is mean total number of steps taken per day?

```r
activity_sum <- aggregate(activity$steps ~ activity$date,FUN=sum)
colnames(activity_sum) <- c("date","steps")
mean_perday <- mean(activity_sum$steps)
median_perday <- median(activity_sum$steps)
hist(activity_sum$steps,xlab="Total Steps",col="red", main="Total Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean of Steps taken per day is: 10766.19

The median of Steps taken per day is: 10765

(As per instructions nulls and zeros have been ignored for now)

### What is the Average Daily Activity Pattern?


```r
activity_interval_mean <- aggregate(activity$steps ~ activity$interval,FUN=mean)
colnames(activity_interval_mean) <- c("interval","steps")
plot(activity_interval_mean$interval, activity_interval_mean$steps, type='l', col="blue", main ="Average Number of Steps Over Days", xlab="Interval", ylab="Avg. Number of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
max_avg_int <- activity_interval_mean[which.max(activity_interval_mean$steps),]
```

The 5 min interval that contains Max. Number of Steps is: 835, 206.17

### Imputing Missing Values


```r
total_nas <- nrow(activity[is.na(activity$steps) == TRUE,])
activity$steps[is.na(activity$steps) == TRUE] <- 0
activity_sum_fill <- aggregate(activity$steps ~ activity$date,FUN=sum)
colnames(activity_sum_fill) <- c("date","steps")
hist(activity_sum_fill$steps,xlab="Total Steps (NAs replaced by 0)",col="red", main="Total Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
mean_perday_fill <- mean(activity_sum_fill$steps)
median_perday_fill <- median(activity_sum_fill$steps)
```

Total Number of NAs in the dataset are: 2304

NAs are then replaced by 0

The mean of Steps taken per day is (NAs are imputed as 0): 9354.23

The median of Steps taken per day is (NAs are imputed as 0): 10395

This differs from non imputed values as below:

The mean of Steps taken per day is (NAs are not imputed): 10766.19

The median of Steps taken per day is (NAs are not imputed): 10765

### Are there differences in activity patterns between weekdays and weekends?

```r
activity$date <- as.Date(activity$date,"%Y-%m-%d")
activity$day <- weekdays(activity$date)
activity$day_type[activity$day == "Sunday" | activity$day == "Saturday"] <- "Weekend"
activity$day_type[is.na(activity$day_type) == TRUE] <- "Weekday"
activity$day_type <- as.factor(activity$day_type)
activity_weekdays <- aggregate(steps ~ interval+day_type, activity, mean)
activity_weekend <- activity_weekdays[activity_weekdays$day_type == "Weekend",]
activity_weekday <- activity_weekdays[activity_weekdays$day_type == "Weekday",]
par(mfcol = c(1,2))
plot(activity_weekend$steps ~ activity_weekend$interval,type="l", col = "Blue",ylab="Steps",xlab ="Interval", main="Weekend Activity")
plot(activity_weekday$steps ~ activity_weekday$interval,type="l",col = "red", ylab="Steps",xlab="Interval", main="Weekday Activity")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


