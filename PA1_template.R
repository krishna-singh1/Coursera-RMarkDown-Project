library(plyr)
library(ggplot2)
library(lattice)
health<-read.csv("activity.csv",na.strings = "NA",sep = ",",header = TRUE)
View(health)
names(health)
summary(health)
head(health)

#converting date variable to date class
health$date<-as.Date(health$date,format= "%Y-%m-%d")
health$interval<-factor(health$interval)

#ignore the missing case
ignoreNa<- is.na(as.character(health$steps))
data_no_NA<- health[!ignoreNa,]
View(data_no_NA)

#
#aggregation the number of steps taken each day

steps_each_day<-aggregate(steps~date,data =data_no_NA,sum )
View(steps_each_day)
head(steps_each_day)

# create histogram 
hist(steps_each_day$steps,breaks = 20,col ="red",xlab = "number of steps",ylab="Date",
     main="histogram of total nnumber of steps taken each day")

#Mean and median number of steps taken each day
mean(steps_each_day$steps)

median(steps_each_day$steps)

#Time series plot of the average number of steps taken
steps_per_interval<- aggregate(data_no_NA$steps,by=list(interval=data_no_NA$interval),FUN=mean)
head(steps_per_interval)
colnames(steps_per_interval)<-c("interval","average_steps")

plot(as.integer(levels(steps_per_interval$interval)),steps_per_interval$average_steps,type = "l",
     xlab = "Interval",ylab = "Average Number of Steps",main = "Average Daliy Activity pattern",col="blue")

#maximum number of average of steps
maxNumberSteps<- max(steps_per_interval$average_steps)
maxNumberSteps


#The 5-minute interval that, on average, contains the maximum number of steps
max_step<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
max_step

#imputing missing value
sum(is.na(as.character(health$steps)))

#for "date" variable
sum(is.na(as.character(health$date)))

#for interval vlaue
sum(is.na(as.character(health$interval)))

#histogram
hist(as.numeric(steps_each_day$steps),breaks = 20,col="red", xlab = "Number of steps",main = "histogram of total number taken each day")

# create a factor variable to stor the day of week
data_no_NA$day <- as.factor(weekdays(data_no_NA$date))
table(data_no_NA$day)

#creating coa logical variable for weekdays and weekend
data_no_NA$is_weekday<- ifelse(!(data_no_NA$day %in% c("Saturday","Sunday")),TRUE,FALSE)
table(data_no_NA$is_weekday)

#calculatting average number of steps in week days
weekdays_data<- data_no_NA[!data_no_NA$is_weekday,]
steps_per_interval_weekdays<-aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval),FUN=mean)

#calculating the average number of steps for weekends
weekends_data<- data_no_NA[!data_no_NA$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps,by=list(interval=weekends_data$interval),FUN=mean)


#Adding columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")
#Adding a column to indecate the day
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

#merging the tow together
week_data <-rbind(steps_per_interval_weekdays,steps_per_interval_weekends)
week_data$day <- as.factor(week_data$day)

#making plot
xyplot(average_steps ~ interval|day,type="l",data = week_data,layout=c(1,2),
       ylab="Number of steps")
