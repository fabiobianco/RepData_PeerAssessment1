# Reproducible Research: Peer Assessment 1


```r
library(readr)
library(dplyr)
```

## Loading and preprocessing the data


```r
activity <- read_csv("~/R Workspace/RepData_PeerAssessment1/activity.csv")
dim(activity) 
```

```
## [1] 17568     3
```

```r
head(activity)
```

```
## # A tibble: 6 × 3
##   steps       date interval
##   <int>     <date>    <int>
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

### 1 -Calculate the total number of steps taken per day

```r
y <- group_by(activity, date)
tot_steps <- summarise(y, sum(steps))
head(tot_steps)
```

```
## # A tibble: 6 × 2
##         date `sum(steps)`
##       <date>        <int>
## 1 2012-10-01           NA
## 2 2012-10-02          126
## 3 2012-10-03        11352
## 4 2012-10-04        12116
## 5 2012-10-05        13294
## 6 2012-10-06        15420
```

## Calculate and report the mean and median of the total number of steps taken per day


```r
z <- group_by(activity, date)
mean_steps <- summarise(z, mean(steps))
median_steps <- summarise(z, median(steps))
head(mean_steps)
```

```
## # A tibble: 6 × 2
##         date `mean(steps)`
##       <date>         <dbl>
## 1 2012-10-01            NA
## 2 2012-10-02       0.43750
## 3 2012-10-03      39.41667
## 4 2012-10-04      42.06944
## 5 2012-10-05      46.15972
## 6 2012-10-06      53.54167
```

```r
head(median_steps)
```

```
## # A tibble: 6 × 2
##         date `median(steps)`
##       <date>           <dbl>
## 1 2012-10-01              NA
## 2 2012-10-02               0
## 3 2012-10-03               0
## 4 2012-10-04               0
## 5 2012-10-05               0
## 6 2012-10-06               0
```

## Calculate the total number of steps taken per day

```r
w <- group_by(activity, date)
median_steps <- summarise(w, median(steps))
head(median_steps)
```

```
## # A tibble: 6 × 2
##         date `median(steps)`
##       <date>           <dbl>
## 1 2012-10-01              NA
## 2 2012-10-02               0
## 3 2012-10-03               0
## 4 2012-10-04               0
## 5 2012-10-05               0
## 6 2012-10-06               0
```

## What is the average daily activity pattern?



## Imputing missing values



```r
x <- activity[!is.na(activity$steps),]
dim(x)
```

```
## [1] 15264     3
```


## Are there differences in activity patterns between weekdays and weekends?
