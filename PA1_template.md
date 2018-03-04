---
title: "RMarkDown"
author: "Krrish"
date: "March 4, 2018"
output:
  html_document:
    keep_md: true
---


##importing library

```r
library(plyr)
library(ggplot2)
library(lattice)
```

##reading csv

```r
health<-read.csv("activity.csv",na.strings = "NA",sep = ",",header = TRUE)
names(health)
```

```
## [1] "steps"    "date"     "interval"
```

```r
summary(health)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
head(health)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```
##converting date variable to date class

```r
health$date<-as.Date(health$date,format= "%Y-%m-%d")
health$interval<-factor(health$interval)
```
##ignore the missing case

```r
ignoreNa<- is.na(as.character(health$steps))
data_no_NA<- health[!ignoreNa,]
```

##aggregation the number of steps taken each day

```r
steps_each_day<-aggregate(steps~date,data =data_no_NA,sum )
head(steps_each_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

## create histogram 

```r
hist(steps_each_day$steps,breaks = 20,col ="red",xlab = "number of steps",ylab="Date",
     main="histogram of total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
##Mean and median number of steps taken each day

```r
mean(steps_each_day$steps)
```

```
## [1] 10766.19
```

```r
median(steps_each_day$steps)
```

```
## [1] 10765
```
##Time series plot of the average number of steps taken

```r
steps_per_interval<- aggregate(data_no_NA$steps,by=list(interval=data_no_NA$interval),FUN=mean)
head(steps_per_interval)
```

```
##   interval         x
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
colnames(steps_per_interval)<-c("interval","average_steps")

plot(as.integer(levels(steps_per_interval$interval)),steps_per_interval$average_steps,type = "l",
     xlab = "Interval",ylab = "Average Number of Steps",main = "Average Daliy Activity pattern",col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

####maximum number of average of steps

```r
maxNumberSteps<- max(steps_per_interval$average_steps)
maxNumberSteps
```

```
## [1] 206.1698
```

####The 5-minute interval that, on average, contains the maximum number of steps

```r
max_step<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
max_step
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```
##imputing missing value

```r
sum(is.na(as.character(health$steps)))
```

```
## [1] 2304
```
##for "date" variable

```r
sum(is.na(as.character(health$date)))
```

```
## [1] 0
```
##for interval vlaue

```r
sum(is.na(as.character(health$interval)))
```

```
## [1] 0
```
##histogram

```r
hist(as.numeric(steps_each_day$steps),breaks = 20,col="red", xlab = "Number of steps",main = "histogram of total number taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

## create a factor variable to stor the day of week

```r
data_no_NA$day <- as.factor(weekdays(data_no_NA$date))
table(data_no_NA$day)
```

```
## 
##    Friday    Monday  Saturday    Sunday  Thursday   Tuesday Wednesday 
##      2016      2016      2016      2016      2304      2592      2304
```
##creating coa logical variable for weekdays and weekend

```r
data_no_NA$is_weekday<- ifelse(!(data_no_NA$day %in% c("Saturday","Sunday")),TRUE,FALSE)
table(data_no_NA$is_weekday)
```

```
## 
## FALSE  TRUE 
##  4032 11232
```

##calculatting average number of steps in week days

```r
weekdays_data<- data_no_NA[!data_no_NA$is_weekday,]
steps_per_interval_weekdays<-aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval),FUN=mean)
```
##calculating the average number of steps for weekends

```r
weekends_data<- data_no_NA[!data_no_NA$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps,by=list(interval=weekends_data$interval),FUN=mean)
```


##Adding columns names

```r
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
```
##Adding a column to indecate the day

```r
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"
```
##merging the tow together

```r
week_data <-rbind(steps_per_interval_weekdays,steps_per_interval_weekends)
week_data$day <- as.factor(week_data$day)
```
##making plot

```r
xyplot(average_steps ~ interval|day,type="l",data = week_data,layout=c(1,2),
       ylab="Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
