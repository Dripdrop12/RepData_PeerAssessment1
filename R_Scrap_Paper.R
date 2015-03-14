setwd("C:/Users/Jon/Desktop/RepData_PeerAssessment1")
activity <- read.csv("activity.csv",header=TRUE)
activity$date <- as.Date(activity$date)

activityDay <- activity %>%
        group_by(date) %>%
        summarize (stepsDay = sum(steps)
                   )

ggplot(data=activityDay,mapping = aes(date,stepsDay))+
        geom_bar(stat="identity", fill ="dark blue")+
        labs(y="Steps per Day", x="Date", title="Histogram of Steps per Day")

activityInt <- activity %>%
        group_by(interval) %>%
        summarize (stepsInt = sum(steps, na.rm=TRUE)/61
        )

maxInt <- activityInt$interval[which.max(x = activityInt$stepsInt)]

ggplot(data =activityInt,mapping=aes(interval,stepsInt))+
        geom_line()+
        labs(y="Average steps per interval",x="5-Minute Interval",title="Time Series Graph")

missingval <- sum(is.na(activity$steps))


mergedactivity <- merge(activityInt,activity,by="interval")
mergedactivity$steps <- ifelse(is.na(mergedactivity$steps),
                         mergedactivity$stepsInt,
                         mergedactivity$steps)
mergedactivity$stepsInt <- NULL

cleanhist <- mergedactivity %>%
        group_by(date) %>%
        summarize (stepsDay = sum(steps)
        )

ggplot(data=cleanhist,mapping = aes(date,stepsDay))+
        geom_bar(stat="identity", fill ="dark blue")+
        labs(y="Steps per Day", x="Date", title="Histogram of Steps per Day")
cleanmean <- mean(cleanhist$stepsDay)
cleanmedian <- median(cleanhist$stepsDay)

mergedactivity$dow <- weekdays(mergedactivity$date)
mergedactivity$weekend <- ifelse(mergedactivity$dow == weekend,"weekend","weekday")
mergedactivity$weekend <- as.factor(mergedactivity$weekend)

cleanInt <- mergedactivity %>%
        group_by(interval,weekend) %>%
        summarize (stepsInt = sum(steps, na.rm=TRUE)
        )

ggplot(data =cleanInt,mapping=aes(interval,stepsInt))+
        geom_line()+
        facet_grid(weekend~.)+
        labs(y="Total steps per interval",x="5-Minute Interval",title="Time Series Graph")



