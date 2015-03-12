file4analysisactivity<-function(){
  library(lubridate)
  library(dplyr)
  ##  Loading and preprocessing the data
  ## Transform date variable (factor) into date value
  file<-read.csv("./activity-data/activity.csv", header=TRUE , sep=",")
  file$date<-ymd(file$date)
  ## Compute total number of steps taken per day
  ## Make histogram of the number of steps taken each day
  ## Compute and return mean and median
  file2<- file %>% group_by(date) %>% summarize(steps=sum(steps, na.rm=TRUE))
  hist(file2$steps, main="Histogram of number of steps taken each day", xlab="Number of steps")
  print(paste("Mean =", mean(file2$steps)))
  print(paste("Median =", median(file2$steps)))
  ## Average daily activity pattern
  ## Plot time serie
  ## Determine maximum interval
  file3<- file %>% group_by(interval) %>% summarize(steps=mean(steps, na.rm=TRUE))
  plot(file3$interval, file3$steps, xlab="Time intervals", ylab="Number of steps", main = "Average number of steps taken, averaged across all days", type="l")
  print(paste("Interval with maximum number of steps =", file3$interval[file3$steps==max(file3$steps)]))
  ## Imputing missing values
  ## Calculate and report the total number of missing values
  ## Fill missing values with median by interval
  ## File4 is the new data set with filled in missing values
  ## Create histogram of new number of steps and compute mean and median
  ## Explain difference in results
  print(paste("Total number of missing values =", sum(is.na(file)) ))
  v<-which(is.na(file$steps))
  file4<-file
  for (i in v) {
    file4$steps[i]<-file3$steps[file3$interval==file4$interval[i]]
    }
  file5<- file4 %>% group_by(date) %>% summarize(steps=sum(steps, na.rm=TRUE))
  hist(file5$steps, main="Histogram of number of steps taken each day", xlab="Number of steps")
  print(paste("Mean =", mean(file5$steps)))
  print(paste("The difference of the mean before and after imputing missing data is ", mean(file5$steps-mean(file2$steps))))
  print(paste("Median =", median(file5$steps)))
  print(paste("The difference of the median before and after imputing missing data is ", median(file5$steps-median(file2$steps))))
  print("Imputing missing data had the effect to normalize a bit the data set")
  ## Difference between weekday and weekend
  ## Create new variable
  ## Make panel plot of average  by interval
  file4$daytype<-as.factor(ifelse(weekdays(file4$date)=="samedi"|weekdays(file4$date)=="dimanche", "weekend","weekday"))
  file6<- file4 %>% group_by(daytype, interval) %>% summarize(steps=mean(steps, na.rm=TRUE))
  par(mfrow=c(2,1))
  library(lattice)
  xyplot(steps~interval|daytype, layout=c(1,2), data=file6, type="l")
}