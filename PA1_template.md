---
title: "Reproducible research - week2.rmd"
author: "Raghu Maan"
date: "3/22/2019"
output:
  html_document: default
  pdf_document: default
  keep_md: true
---



## Reproducible research - week 2 assignment

###1. Code for reading in the dataset and/or processing the data


```r
setwd("/Users/t93ku6h/Documents/Development/coursera/Reproducible-Research/week2")
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activity_data <- read.csv('activity.csv')
```

####Summary

```r
summary(activity_data)
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

###2. Histogram of the total number of steps taken each day

```r
#install.packages("ggplot2")
library(ggplot2)
steps_per_day <- tapply(activity_data$steps, activity_data$date, sum, na.rm=TRUE)
qplot(steps_per_day, xlab='Steps per day', ylab='Frequency', binwidth=1000)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
png_file <- "total-steps.png"
dev.print(png, file = png_file, width = 400, height = 400)
```

```
## RStudioGD 
##         2
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
summary(steps_per_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

###4. Time series plot of the average number of steps taken

```r
library(dplyr)
#split the activity data by time internval (0,5,10..) and run the mean function over the splitted data
mean_steps_per_interval <- aggregate(x=list(steps=activity_data$steps), by=list(interval=activity_data$interval), FUN=mean, na.rm=TRUE)
head(mean_steps_per_interval)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
#plot interval by steps
with(mean_steps_per_interval,plot(interval,steps,type="l",ylab="average number of steps",xlab="5-minute time interval"))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
png_file <- "average-steps.png"
dev.print(png, file = png_file, width = 400, height = 400)
```

```
## RStudioGD 
##         2
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

###5. The 5-minute interval that, on average, contains the maximum number of steps
mean_steps_per_interval[which.max(mean_steps_per_interval$steps),"interval"]

###6. Code to describe and show a strategy for imputing missing data 

```r
#install.packages("mice")
library(mice)
#show missing data - shows 2304 missing entries
md.pattern(activity_data,plot=TRUE)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```
##       date interval steps     
## 15264    1        1     1    0
## 2304     1        1     0    1
##          0        0  2304 2304
```

```r
#impute data with pmm method
#https://statisticalhorizons.com/predictive-mean-matching
activity_data_imputed <- mice(activity_data,m=1,method = "pmm")
```

```
## 
##  iter imp variable
##   1   1  steps
##   2   1  steps
##   3   1  steps
##   4   1  steps
##   5   1  steps
```

```
## Warning: Number of logged events: 5
```

###7. Histogram of the total number of steps taken each day after missing values are imputed

```r
activity_data_imputed_completed <- complete(activity_data_imputed,1)
steps_per_day <- tapply(activity_data_imputed_completed$steps, activity_data_imputed_completed$date, sum)
qplot(steps_per_day, xlab='Steps per day', ylab='Frequency', binwidth=1000)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
png_file <- "total-steps-imputed.png"
dev.print(png, file = png_file, width = 400, height = 400)
```

```
## RStudioGD 
##         2
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

###8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
activity_data$day <-  ifelse(as.POSIXlt(activity_data$date)$wday %in% c(0,6), 'weekend', 'weekday')
activity_data_with_dayofweek <- aggregate(steps ~ interval + day, data=activity_data, mean)
ggplot(activity_data_with_dayofweek, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(day ~ .) +
    xlab("5-minute time interval") + 
    ylab("avarage number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
png_file <- "avearge-steps-by-dayofweek.png"
dev.print(png, file = png_file, width = 400, height = 400)
```

```
## RStudioGD 
##         2
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```
