## Peer-graded Assignment: Reproducible Research Week 2 Course Proejct 1


## Loading and preprocessing the data 
## 1. Download file and load the dataset to R
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
f <- file.path(getwd(), "activity.zip")
download.file(fileUrl, f, curl = "method")
unzip(zipfile = "activity.zip")

## 2. Process/transform the data (if necessary) into a format suitable for your analysis
activity_monitoring_data <- read.csv("activity.csv")
activity_monitoring_data$date <- as.Date(activity_monitoring_data$date, "%Y-%m-%d")


## What is mean total number of steps taken per day?
## 1. Calculate the total number of steps taken per day
## load ggplot2
library(ggplot2)

steps_per_day <- split(activity_monitoring_data, activity_monitoring_data$date)
total_steps <- sapply(steps_per_day, function(x) {
  sum(x[, "steps"])
})
total_steps_df <- as.data.frame(total_steps)

## 2. If you do not understand the difference between a histogram and a barplot, research the
## difference between them. Make a histogram of the total number of steps taken each day


## Plot a histogram of the total number of steps taken each day
ggplot(total_steps_df, aes(total_steps)) + geom_histogram(binwidth = 1000, fill = "white", col = "blue") +
  labs(x= "Steps", y = "Frequency", title = "Total Number of Steps Taken each Day (Histogram)")

## 3. Calculate and report the mean and median of the total number of steps taken per day
## Mean of the total number of steps taken per day
mean(total_steps_df$total_steps, na.rm = TRUE)
## Median of the total number of steps taken per day
median(total_steps_df$total_steps, na.rm = TRUE)


## What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all days (y-axis)
library(dplyr)

avg_daily_steps <- activity_monitoring_data %>% group_by(interval) %>%
  summarize(avg_steps = mean(steps, na.rm = TRUE))

## Plot a time series plot
ggplot(avg_daily_steps, aes(interval, avg_steps)) + geom_line(col = "orange") + 
  labs(x = "5-minute intervals", y = "Average Number of Steps", title = "Average Daily Steps Across All Days")

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
subset(avg_daily_steps, avg_steps == max(avg_steps))


## Imputing missing values
## Note that there are a number of days/intervals where there are missing values (coded as NA). 
## The presence of missing days may introduce bias into some calculations or summaries of the data.
## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
total_number_missing_values <- subset(activity_monitoring_data, is.na(activity_monitoring_data))
nrow(total_number_missing_values)

## 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
## For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

## Strategy for missing values: The average of the associated intervals will be used to fill in all of the missing values in the dataset.
## Fill the NA values with the average steps dataset obtained from "avg_daily_steps".

## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity_monitoring_data_no_NA <- left_join(activity_monitoring_data, avg_daily_steps, by = "interval") %>%
  transmute(steps = coalesce(steps, avg_steps), date, interval) 

## 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
## median total number of steps taken per day. Do these values differ from the estimates from the first part of 
## the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


## Calculate the total number of steps taken each day with "activity_monitoring_data_no_NA"
steps_per_day_no_NA <- split(activity_monitoring_data_no_NA, activity_monitoring_data_no_NA$date)
total_steps_no_NA <- sapply(steps_per_day_no_NA, function(x) {
  sum(x[, "steps"])
})
total_steps_no_NA_df <- as.data.frame(total_steps_no_NA)
names(total_steps_no_NA_df) <- c("steps_no_NA")
total_steps_no_NA_df$steps_no_NA <- as.integer(total_steps_no_NA_df$steps_no_NA)

## Plot a histogram of the total number of steps taken each day
ggplot(total_steps_no_NA_df, aes(steps_no_NA)) + geom_histogram(binwidth = 1000, fill = "white", col = "red") +
  labs(x = "Steps", y = "Frequency", title = "Total Number of Steps Taken each Day w/o NAs (Histogram)")

## Calculate and report mean and median total number of steps taken per day
mean(total_steps_no_NA_df$steps_no_NA)
median(total_steps_no_NA_df$steps_no_NA)

## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

## We have the new values for mean and median. The new mean is 10766.16 and the old one is 10766.19. 
## The new median is 10766 and the old one is 10765. It is notable that the missing values were removed with "na.rm = TRUE" 
## when the values are calculated from the first part of the assignment
## We filled out the NAs with average of associated intervals, the new means slightly decreased while the new median increased a little bit.
## The presence of missing days may introduce bias into some calculations and they are compensated by filling out the NAs with the average.
  
new_mean <- mean(total_steps_no_NA_df$steps_no_NA)
old_mean <- mean(total_steps_df$total_steps, na.rm = TRUE)
new_median <- median(total_steps_no_NA_df$steps_no_NA)
old_median <- median(total_steps_df$total_steps, na.rm = TRUE)

comparison <- data.frame(mean = c(new_mean, old_mean), median = c(new_median, old_median)) 
rownames(comparison) <- c("new value", "old value")
print(comparison)


## Are there differences in activity patterns between weekdays and weekends?
## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
## 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity_DoW <- activity_monitoring_data_no_NA
activity_DoW$day_of_week <- weekdays(activity_DoW$date)
activity_DoW$weekday_weekend <- ifelse(activity_DoW$day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_DoW$weekday_weekend <- as.factor(activity_DoW$weekday_weekend)

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all weekday days or weekend days(y-axis). 
## See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
avg_daily_steps_wkday <- activity_DoW %>% filter(weekday_weekend == "weekday") %>% group_by(interval) %>%
  summarize(average_steps = mean(steps)) 
avg_daily_steps_wkday$day <- "weekday"

avg_daily_steps_wkend <- activity_DoW %>% filter(weekday_weekend == "weekend") %>% group_by(interval) %>%
  summarize(average_steps = mean(steps))
avg_daily_steps_wkend$day <- "weekend"

avg_daily_steps_total <- rbind(avg_daily_steps_wkday, avg_daily_steps_wkend)

ggplot(avg_daily_steps_total, aes(interval, average_steps)) + geom_line(aes(color = day)) + facet_grid(rows = vars(day)) + 
  labs(x = "5-minute intervals", y = "Average Number of Steps", title = "Average Daily Steps Across All weekdays or weekends")

