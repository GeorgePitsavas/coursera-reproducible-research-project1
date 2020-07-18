Sys.setlocale("LC_TIME","English United States")

library(readr)
A<- read_csv("activity.csv")
str(A)



AstepsPerDay= tapply(A$steps,A$date,FUN = sum,na.rm=T)
AstepsPerDay

library(ggplot2)
p=qplot(AstepsPerDay,binwidth=1000,xlab="total number of steps taken each day")
print(p)

AstepsMean = mean(AstepsPerDay,na.rm = T)
AstepsMean

AstepsMedian=median(AstepsPerDay,na.rm = T)
AstepsMedian


interval_means= aggregate(x=list(steps=A$steps),by=list(interval=A$interval),FUN=mean,na.rm=T)
p=ggplot(interval_means,aes(x=interval,y=steps))
p+geom_line()+labs(title =" Time series plot of average number of steps", x = "interval", y = "steps") 

interval_means[which.max(interval_means$steps),]


sum(is.na(A))

A$steps[is.na(A$steps)]=mean(A$steps,na.rm=T)
head(A)

AstepsPerDay2= tapply(A$steps,A$date,FUN = sum,na.rm=T)
head(AstepsPerDay2)

AstepsMean2 = mean(AstepsPerDay2,na.rm = T)
AstepsMean2
c(AstepsMean,AstepsMean2)

AstepsMedian2=median(AstepsPerDay2,na.rm = T)
AstepsMedian2
c(AstepsMedian,AstepsMean2)

p=qplot(AstepsPerDay2,binwidth=1000,xlab="total number of steps taken each day")
print(p)


#convert data into weekdays
Sys.setlocale("LC_TIME", "C")
weekdays(Sys.Date()+0:6)
A$days= tolower(weekdays(A$date))

#catigorised day into weekend and weekdays
A$day_type=ifelse(A$days=="saturday"| A$days=="sunday","weekend","weekday")
head(A)

#average steps on weekend or weekday in the intervals
avg_steps=aggregate(A$steps,by=list(A$interval,A$day_type),FUN=mean,na.rm=T)
colnames(avg_steps)=c("interval","day_type","steps")

p=ggplot(avg_steps,aes(x=interval,y=steps,color=day_type))
p+geom_line()+facet_grid(day_type~.)

       