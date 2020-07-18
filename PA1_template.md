---
output: 
  html_document:
    keep_md: true
---
---
title: "PA1_template"
author: "George Pitsavas"
date: "18/7/2020"
output:
  pdf_document: default
  html_document: default
  ---

# Importing data


```r
library(readr)
A<- read_csv("activity.csv")
```

```
## Parsed with column specification:
## cols(
##   steps = col_double(),
##   date = col_date(format = ""),
##   interval = col_double()
## )
```

```r
str(A)
```

```
## tibble [17,568 x 3] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ steps   : num [1:17568] NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date[1:17568], format: "2012-10-01" "2012-10-01" ...
##  $ interval: num [1:17568] 0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

# Calculate the total number of steps taken per day


```r
AstepsPerDay= tapply(A$steps,A$date,FUN = sum,na.rm=T)
AstepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0        126      11352      12116      13294      15420      11015 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0      12811       9900      10304      17382      12426      15098 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##      10139      15084      13452      10056      11829      10395       8821 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##      13460       8918       8355       2492       6778      10119      11458 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##       5018       9819      15414          0      10600      10571          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##      10439       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##      10765       7336          0         41       5441      14339      15110 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##       8841       4472      12787      20427      21194      14478      11834 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##      11162      13646      10183       7047          0
```

# Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
p=qplot(AstepsPerDay,binwidth=1000,xlab="total number of steps taken each day")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

# Calculate and report the mean and median of the total number of steps taken per day

```r
AstepsMean = mean(AstepsPerDay,na.rm = T)
AstepsMean
```

```
## [1] 9354.23
```

```r
AstepsMedian=median(AstepsPerDay,na.rm = T)
AstepsMedian
```

```
## [1] 10395
```

# a time series plot  of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
interval_means= aggregate(x=list(steps=A$steps),by=list(interval=A$interval),FUN=mean,na.rm=T)
p=ggplot(interval_means,aes(x=interval,y=steps))
p+geom_line()+labs(title =" Time series plot of average number of steps", x = "interval", y = "steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval_means[which.max(interval_means$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

# the total number of missing values in the dataset

```r
sum(is.na(A))
```

```
## [1] 2304
```

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.

```r
A$steps[is.na(A$steps)]=mean(A$steps,na.rm=T)
head(A)
```

```
## # A tibble: 6 x 3
##   steps date       interval
##   <dbl> <date>        <dbl>
## 1  37.4 2012-10-01        0
## 2  37.4 2012-10-01        5
## 3  37.4 2012-10-01       10
## 4  37.4 2012-10-01       15
## 5  37.4 2012-10-01       20
## 6  37.4 2012-10-01       25
```

# new dataset that is equal to the original dataset but with the missing data filled in

```r
AstepsPerDay2= tapply(A$steps,A$date,FUN = sum,na.rm=T)
head(AstepsPerDay2)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00
```

# report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
AstepsMean2 = mean(AstepsPerDay2,na.rm = T)
AstepsMean2
```

```
## [1] 10766.19
```

```r
c(AstepsMean,AstepsMean2)
```

```
## [1]  9354.23 10766.19
```

```r
AstepsMedian2=median(AstepsPerDay2,na.rm = T)
AstepsMedian2
```

```
## [1] 10766.19
```

```r
c(AstepsMedian,AstepsMean2)
```

```
## [1] 10395.00 10766.19
```

# histogram of the total number of steps taken each day

```r
p=qplot(AstepsPerDay2,binwidth=1000,xlab="total number of steps taken each day")
print(p)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

# convert data into weekdays

```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
weekdays(Sys.Date()+0:6)
```

```
## [1] "Saturday"  "Sunday"    "Monday"    "Tuesday"   "Wednesday" "Thursday" 
## [7] "Friday"
```

```r
A$days= tolower(weekdays(A$date))
```

# catigorised day into weekend and weekdays

```r
A$day_type=ifelse(A$days=="saturday"| A$days=="sunday","weekend","weekday")
head(A)
```

```
## # A tibble: 6 x 5
##   steps date       interval days   day_type
##   <dbl> <date>        <dbl> <chr>  <chr>   
## 1  37.4 2012-10-01        0 monday weekday 
## 2  37.4 2012-10-01        5 monday weekday 
## 3  37.4 2012-10-01       10 monday weekday 
## 4  37.4 2012-10-01       15 monday weekday 
## 5  37.4 2012-10-01       20 monday weekday 
## 6  37.4 2012-10-01       25 monday weekday
```

# average steps on weekend or weekday in the intervals

```r
avg_steps=aggregate(A$steps,by=list(A$interval,A$day_type),FUN=mean,na.rm=T)
colnames(avg_steps)=c("interval","day_type","steps")
```

# Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
p=ggplot(avg_steps,aes(x=interval,y=steps,color=day_type))
p+geom_line()+facet_grid(day_type~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->
