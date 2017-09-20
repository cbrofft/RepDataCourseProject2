library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
## Staging the files and setting the directory.  
##The file will be downloaded from the source and unzipped and saved as activity.csv
setwd("~/Rep_Data_Assignment_1")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
downloadFile <- "./repdata%2Fdata%2Factivity.zip"
activityfile <- "./activity.csv"
download.file(fileURL, downloadFile, method = "curl")
unzip(downloadFile, overwrite = T, exdir = "./")
unlink(downloadFile)

## Create activity data frame and change date from factor to date type (will be needed later)
activity <-read.csv(activityfile, header=T, sep=",")

activity$date <- as.Date(activity$date, "%Y-%m-%d")

# Create data frame with steps by date summed up
dailysteps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)

# Histogram for steps per day
hist(dailysteps$steps, 
     main="Daily Steps", 
     xlab="Steps per Day", 
     ylab = "Interval",
     col = "blue",
     breaks=50)
# Get mean steps
meansteps <- mean(dailysteps$steps)
meansteps
# Get median steps
mediansteps <- median(dailysteps$steps)
mediansteps

# Get daily pattern
dailypattern <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

#Plot the daily pattern of steps
plot(x = dailypattern$interval, 
     y = dailypattern$steps, 
     type = "l", 
     col = "blue",
     xlab = "Intervals",
     ylab = "Average Steps Taken a day",
     main = "Average Daily Activity Pattern")

# Max steps
maxsteps <- dailypattern$interval[which.max(dailypattern$steps)]
maxsteps

# Understand missing values
missingvalues <- sum(is.na(activity$steps))
missingvalues

# Create activity2 file that excludes Nas
activity2 <- activity
nas <- is.na(activity2$steps)
avg_interval <- tapply(activity2$steps, activity2$interval, mean, na.rm=TRUE, simplify = TRUE)
activity2$steps[nas] <- avg_interval[as.character(activity2$interval[nas])]

sum(is.na(activity2))

# Daily steps without NAs
dailysteps2 <- aggregate(steps ~ date, data = activity2, FUN = sum, na.rm = TRUE)
dailysteps2

# Histogram that excludes NAs
hist(dailysteps2$steps, 
     main = "Daily Steps (no-NA)", 
     xlab = "Number of Steps per Day", 
     ylab = "Interval",
     col="red",
     breaks=50)

#Compare summaries to see how NAs effect the data points
summary(dailysteps)

summary(dailysteps2)

head(activity2)

# Break the actvity data into type of days for further analysis
activity2<- activity2%>%
  mutate(typeofday= ifelse(weekdays(activity2$date)=="Saturday" | weekdays(activity2$date)=="Sunday", "Weekend", "Weekday"))
head(activity2)

# Five minute intervals analysis
fivemin2<- aggregate(steps ~ interval, data = activity2, FUN = mean, na.rm = TRUE)
head(fivemin2)

# Plot steps per each type of day
ggplot(activity2, aes(x =interval , y=steps, color=typeofday)) +
  geom_line() +
  labs(title = "Daily Steps Avg (type of day)", x = "Interval", y = "Total Steps") +
  facet_wrap(~ typeofday, ncol = 1, nrow=2)


