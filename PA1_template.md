# Reproducible Research: Peer Assessment 1

```r
# set global chunk options: 
library(knitr)
opts_chunk$set( cache=TRUE , cache.path = 'PA1_template_files/', fig.path='PA1_template_files/figure-html/') 
```

## Loading and preprocessing the data


```r
# Get Data
setwd("activity")
amd <- read.csv("activity.csv" , header = TRUE)
setwd("..")

# Change date class from factor of strings to DATE class  
levels(amd$date) <- as.Date(levels(amd$date))
amd$date <- as.Date(amd$date)

#change interval class from integer to factor
amd$interval <- as.factor(amd$interval)

#Remove NA values in amd2
amd2 <- amd[!(is.na(amd$steps)|is.na(amd$date)|is.na(amd$interval)),]
```

## What is mean total number of steps taken per day?


```r
# summarize and plot
library("plyr")
library("ggplot2")
summ <- ddply(amd2, .(date),summarize,steps = sum(steps))
png(file = "Total Daily Steps.png")
q <- qplot(steps, data=summ , geom = "histogram", xlab = "Daily Steps" , 
             ylab = "Frequency" ,main = "Total Daily Steps")+
      geom_histogram(aes(fill = ..count..)) + 
      scale_fill_gradient("Count", low = "green", high = "red")
print(q)
dev.off()
```

```
## pdf 
##   2
```

```r
meansteps <- mean(summ$steps)
mediansteps <- median(summ$steps)
```
The mean of total number of steps 1.0766 &times; 10<sup>4</sup> and meadian is 10765

## What is the average daily activity pattern?

```r
# summarize and plot
library("plyr")
library("ggplot2")
summ <- ddply(amd2, .(interval),summarize,steps = mean(steps))

png(file = "Average Daily Activity Pattern.png")

q <- qplot(interval,steps,data = summ, geom = c("point","line"),xlab = "Daily Time Interval",ylab ="Average steps", main= "Average Daily Activity Pattern")
print(q)
```

```
## geom_path: Each group consist of only one observation. Do you need to adjust the group aesthetic?
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
chk <- summ$steps == max(summ$steps)
summ[chk,]
```

```
##     interval steps
## 104      835 206.2
```
## Imputing missing values


```r
print(c("number of NA in steps :", sum(is.na(amd$steps))))
```

```
## [1] "number of NA in steps :" "2304"
```

```r
print(c("number of NA in date :", sum(is.na(amd$date))))
```

```
## [1] "number of NA in date :" "0"
```

```r
print(c("number of NA in interval :", sum(is.na(amd$interval))))
```

```
## [1] "number of NA in interval :" "0"
```

```r
print("Impute principle for steps : Average daily interval activity" )
```

```
## [1] "Impute principle for steps : Average daily interval activity"
```

```r
library("plyr")
lookup<- ddply(amd2, .(interval),summarize,steps = mean(steps))
newamd <- amd
for (i in 1:nrow(newamd))
    if (is.na(newamd[i,1]))
        newamd[i,1] = lookup[lookup$interval == newamd[i,3],2]
# histogram for number of steps

library("ggplot2")
summ <- ddply(newamd, .(date),summarize,steps = sum(steps))
png(file = "Imputed Data Total Daily Steps.png")
q <- qplot(steps, data=summ , geom = "histogram", xlab = "Daily Steps" , 
             ylab = "Frequency" ,main = "Imputed Data :Total Daily Steps")+
      geom_histogram(aes(fill = ..count..)) + 
      scale_fill_gradient("Count", low = "green", high = "red")
print(q)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
meansteps <- mean(summ$steps)
mediansteps <- median(summ$steps)
```
The mean of total number of steps 1.0766 &times; 10<sup>4</sup> and meadian is 1.0766 &times; 10<sup>4</sup>

## Are there differences in activity patterns between weekdays and weekends?

```r
library("plyr")
summ <- ddply(newamd,.(date,days = weekdays(date)),summarize,steps = sum(steps))
summ <- ddply(summ, .(days),summarize,steps = mean(steps))
x <- data.frame( num =1:7 ,days = c("Sunday", "Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
summ <- merge(summ,x)
summ <- summ[order(summ$num),]
summ[,1:2]
```

```
##        days steps
## 4    Sunday 12089
## 2    Monday 10151
## 6   Tuesday  8950
## 7 Wednesday 11677
## 5  Thursday  8496
## 1    Friday 12006
## 3  Saturday 12314
```
