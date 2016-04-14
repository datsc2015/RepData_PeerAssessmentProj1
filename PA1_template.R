
### Course: Reproducible Research
### Project 1
### Date: 4/10/2016

rm(list=ls())
ls()

setwd('C:/Users/GWANG1/Documents/GitHub/RepData_PeerAssessmentProj1')

# Load packages
require(knitr)
require(markdown)

# Create .md, .html, and .pdf files
knit("PA1_template.Rmd")
markdownToHTML('PA1_template.md', 'PA1_template.html', options=c("use_xhml")) #
#system("pandoc -s PA1_template.html -o PA1_template.pdf")


##### Loading and processing data
dat <- read.csv('activity.csv')
# convert date to date data type

dat$date <- as.Date(dat$date)


######################################
###### Mean of total number of steps taken per day

library(ggplot2)
datc <- na.omit(dat) 
steps.total <-tapply(datc$steps, datc$date,FUN=sum, na.rm=TRUE)
dim(steps.total)
hist(steps.total,breaks=10,xlab="Total Number of Steps Taken per Day", col="grey")
print("Mean of Total number of steps taken per day")
mean(steps.total,na.rm=T)
print("Median of Total number of steps taken per day")
median(steps.total,na.rm=T)

#######################################
##### Average daily activity
head(dat[!is.na(dat$steps),])
avg <- aggregate(x=list(steps=dat$steps),by=list(interval=dat$interval), FUN=mean, na.rm=T)
ggplot(data=avg, aes(x=interval, y=steps))+geom_line(color="blue",size=1)+xlab("5-minute Interval")+ylab("Average Number of Steps Taken")
print("Maximum number of Steps in 5-minute Interval")
avg[which.max(avg$steps),]

######################################
##### Imputing missing values
#1. total number of missing values in dataset
sum(is.na(dat$steps))

#2. Replace missing value with mean for that 5-minute interval
dat.impmiss <- dat
nas<- is.na(dat.impmiss$steps)
avg_int<- tapply(dat.impmiss$steps, dat.impmiss$interval, mean, na.rm=TRUE, simplify = TRUE)
dat.impmiss$steps[nas] <- avg_int[as.character(dat.impmiss$interval[nas])]
names(dat.impmiss)
print("#Missing")
sum(is.na(dat.impmiss))

#Histogram
steps.imp <-tapply(dat.impmiss$steps, dat.impmiss$date,FUN=sum, na.rm=TRUE)
dim(steps.imp)
hist(steps.imp,breaks=10,xlab="Total Number of Steps Taken per Day", col="green",main="Total Steps including Imputing Value")
mean(steps.imp)
median(steps.imp)

####################################
###### Difference in activity patterns between weekdays and weekends
#Function: wwdat
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

####Plot
avg.imp <- aggregate(steps~interval+weektype, data=dat.impmiss, mean)
#ggplot(avg.imp, aes(x=interval, y=steps, color=weektype))+geom_line()+xlab("Interval")+ylab("Number of Steps")+facet_wrap(~weektype, ncol = 1, nrow=2)
library("lattice")
xyplot(steps ~  interval|factor(weektype), data=avg.imp, 
            type = 'l',
            main="Averaged Across All Weekday Days or Weekend Days",
            xlab="Interval",
            ylab="Number of Steps",layout=c(1,2))


