# Loading and processing data



```r
##### Load packages
require(knitr)
```

```
## Loading required package: knitr
```

```
## Warning: package 'knitr' was built under R version 3.2.4
```

```r
require(markdown)
```

```
## Loading required package: markdown
```

```
## Warning: package 'markdown' was built under R version 3.2.4
```

```r
setwd('C:/Users/GWANG1/Documents/GitHub/RepData_PeerAssessmentProj1')
dat <- read.csv("activity.csv")

#### convert date to date data type
dat$date <- as.Date(dat$date)

library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.4
```

```r
#### Mean of total number of steps taken per day
datc <- na.omit(dat) 
steps.total <-tapply(datc$steps, datc$date,FUN=sum, na.rm=TRUE)
dim(steps.total)
```

```
## [1] 53
```

```r
hist(steps.total,breaks=10,xlab="Total Number of Steps Taken per Day", col="grey",main="Histogram of Total Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)

```r
print("Mean of Total number of steps taken per day")
```

```
## [1] "Mean of Total number of steps taken per day"
```

```r
mean(steps.total,na.rm=T)
```

```
## [1] 10766.19
```

```r
print("Median of Total number of steps taken per day")
```

```
## [1] "Median of Total number of steps taken per day"
```

```r
median(steps.total,na.rm=T)
```

```
## [1] 10765
```

# Average daily activity


```r
head(dat[!is.na(dat$steps),])
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
avg <- aggregate(x=list(steps=dat$steps),by=list(interval=dat$interval), FUN=mean, na.rm=T)
ggplot(data=avg, aes(x=interval, y=steps))+geom_line(color="blue",size=1)+xlab("5-minute Interval")+ylab("Average Number of Steps Taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)

```r
print("Maximum number of Steps in 5-minute Interval")
```

```
## [1] "Maximum number of Steps in 5-minute Interval"
```

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

# Imputing missing values


```r
#### 1. total number of missing values in dataset
sum(is.na(dat$steps))
```

```
## [1] 2304
```

```r
##### 2. Replace missing value with mean for that 5-minute interval
dat.impmiss <- dat
nas<- is.na(dat.impmiss$steps)
avg_int<- tapply(dat.impmiss$steps, dat.impmiss$interval, mean, na.rm=TRUE, simplify = TRUE)
dat.impmiss$steps[nas] <- avg_int[as.character(dat.impmiss$interval[nas])]
names(dat.impmiss)
```

```
## [1] "steps"    "date"     "interval"
```

```r
print("#Missing")
```

```
## [1] "#Missing"
```

```r
sum(is.na(dat.impmiss))
```

```
## [1] 0
```

```r
#### Histogram
steps.imp <-tapply(dat.impmiss$steps, dat.impmiss$date,FUN=sum, na.rm=TRUE)
dim(steps.imp)
```

```
## [1] 61
```

```r
hist(steps.imp,breaks=10,xlab="Total Number of Steps Taken per Day", col="green",main="Total Steps including Imputing Value")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```r
mean(steps.imp)
```

```
## [1] 10766.19
```

```r
median(steps.imp)
```

```
## [1] 10766.19
```


# Difference in activity patterns between weekdays and weekends


```r
#### Function: wwdat
wwdat <- function(date){
        day <-weekdays(date)
        if(day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
                return("Weekday")
        else if( day %in% c("Saturday","Sunday"))
                return("Weekend")
        else
                stop("Invalid Date")
}

dat.impmiss$weektype <- as.Date(dat.impmiss$date)
dat.impmiss$weektype <- sapply(dat.impmiss$weektype, FUN=wwdat)

#### Plot
avg.imp <- aggregate(steps~interval+weektype, data=dat.impmiss, mean)

library("lattice")
xyplot(steps ~  interval|factor(weektype), data=avg.imp, 
            type = 'l',
            main="Averaged Across All Weekday Days or Weekend Days",
            xlab="Interval",
            ylab="Number of Steps",layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

```r
#transform the .Rmd to a markdown (.md) file.
#knit('PA1_template.Rmd')

#transform the .md to HTML format
markdownToHTML("PA1_template.Rmd", "PA1_template.html",fragment.only = TRUE)
```

```
## Warning in readLines(con): incomplete final line found on
## 'PA1_template.Rmd'
```

```r
#transform the  HTML to .md format
markdownToHTML("PA1_template.html","PA1_template.md", fragment.only = TRUE)

knit2html("PA1_template.html","PA1_template.md")
```

```
## 
## 
## processing file: PA1_template.html
```

```
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |.................................................................| 100%
##   ordinary text without R code
```

```
## output file: PA1_template.txt
```

```r
##### Create .md, .html, and .pdf files
#knit2html
#rmarkdown::render('PA1_template.Rmd', 'PA1_template.html')  #, options=c("use_xhml")
#rmarkdown::render('PA1_template.Rmd', 'PA1_template.md')  #, options=c("use_xhml")
```
