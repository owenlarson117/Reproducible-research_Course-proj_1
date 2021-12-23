# Reproducible-research_Course-proj_1
---
"Reproducible Research course proj-1"
author: "Owen Larson"
date: "6/2/2021"
html
---
(originally written in markdown)
##Assignment Instructions
1.Code for reading in the dataset and/or processing the data
2.Histogram of the total number of steps taken each day
3.Mean and median number of steps taken each day
4.Time series plot of the average number of steps taken
5.The 5-minute interval that, on average, contains the maximum number of steps
6.Code to describe and show a strategy for imputing missing data
7.Histogram of the total number of steps taken each day after missing values are imputed
8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report


##Step 1
##Code for reading in the dataset and/or processing the data
```{r, echo = TRUE}
setwd("~/Downloads/Reproducible-research_Course-proj_1")
activity<-read.csv("activity.csv")
```

Exploring the basics of this data
```{r}
dim(activity)
names(activity)
head(activity)
str(activity)
#total number of na
sum(is.na(activity$steps))/dim(activity)[[1]]
library(lubridate)
activity$date<-ymd(activity$date)
length(unique(activity$date))
```


##Step 2
##Histogram of the total number of steps taken each day
```{r, echo = TRUE}
library(ggplot2)
G2<-data.frame(tapply(activity$steps,activity$date,sum,na.rm=TRUE))
G2$date<-rownames(G2)
rownames(G2)<-NULL
names(G2)[[1]]<-"Total Steps"
png("plot1.png")
#Total Steps by date
ggplot(G2,aes(y=G2$`Total Steps`,x=G2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
dev.off()
ggplot(G2,aes(y=G2$`Total Steps`,x=G2$date))+geom_bar(stat="identity") + ylab("Total Steps")+xlab("Date")+ggtitle("Total Steps by date")
#Histogram of total steps
qplot(G2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
png("plot1.1.png")
qplot(G2$`Total Steps`,geom="histogram",xlab="Total Steps",ylab="Counts",main="Total Steps Historgram")
dev.off()
```


##Step 3
##Mean and median number of steps taken each day

```{r, echo = TRUE}
library(dplyr)
G3<-data.frame(round(tapply(activity$steps,activity$date,mean,na.rm=TRUE),2))
G3$date<-rownames(G3)
rownames(G3)<-NULL
names(G3)[[1]]<-"Mean Steps"
temp<-activity%>%select(date,steps) %>% group_by(date) %>% summarise(median(steps))
names(temp)[[2]]<-"Median Steps"
G3$median<-temp$`Median Steps`
G3<-G3 %>% select(date,`Mean Steps`,median)
```

##Step 4
##Time series plot of the average number of steps taken
```{r, echo = TRUE}
G4<-G3
G4$date<-as.Date(G4$date,format="%Y-%m-%d")
ggplot(G4,aes(x=G4$date,y=G4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean-Step Every day")+xlab("Date")+ggtitle("Mean-Step by Date")
png("plot4.png")
ggplot(G4,aes(x=G4$date,y=G4$`Mean Steps`))+geom_bar(stat="identity")+scale_x_date()+ylab("Mean-Step Every day")+xlab("Date")+ggtitle("Mean-Step by Date")
dev.off()
```


##Step 5
##The 5-minute interval that, on average, contains the maximum number of steps

```{r, echo = TRUE}
#This is assuming that the words on average means averaging steps by date and interval
activity$interval<-factor(activity$interval)
G5<-aggregate(data=activity,steps~date+interval,FUN="mean")
G5<-aggregate(data=G5,steps~interval,FUN="max")
```


##Step 6
Code to describe and show a strategy for imputing missing data
There are multiple strategies to deal with multiple value imputations.
The common strategies include:
1. Constant value imputations
2. Regression model value imputations
3. Mean/mode value substitutions
For the purpose of simplicity, in this question, I will use the mean/mode value substitution strategy to impute missing values. That is, using the mean values to substitute out the missing values in the original data set
Before doing any sort of imputation, it is helpful to understand what are the distributions of missing values by date and interval
```{r, echo = TRUE}
G6<-activity
G6$Missing<-is.na(G6$steps)
G6<-aggregate(data=G6,Missing~date+interval,FUN="sum")
G6.1<-data.frame(tapply(G6$Missing,G6$date,sum))
G6.1$date<-rownames(G6.1)
rownames(G6.1)<-NULL
names(G6.1)<-c("Missing","date")
G6.1$date<-as.Date(G6.1$date,format="%Y-%m-%d")
G6.2<-data.frame(tapply(G6$Missing,G6$interval,sum))
G6.2$date<-rownames(G6.2)
rownames(G6.2)<-NULL
names(G6.2)<-c("Missing","Interval")
par(mfrow=c(1,2))
plot(y=G6.1$Missing,x=G6.1$date,main="Missing Value Distribution by Date")
plot(y=G6.2$Missing,x=G6.2$Interval,main="Missing Value Distribution by Interval")
table(activity$date)
```

From the plot we see that the missing values have a pattern. For every interval, there are consistantly 8 missing values. For the date, there are consistantly 288 missing values. And in total, there are 8 dates that have missing value. We don't know the cause for these missing values but there's definitely a pattern. We can also see that the mean value imputation is appropriate.

We can see that every date has 288 data points. It means that the 8 dates have no data points at all what so ever. We can refine the analysis by looking at the missing values depending on their Weekday and interval parameters to match with the average 

```{r, echo = TRUE}
#Dates that have na 
library(lubridate)
G6.3<-as.data.frame(G6.1) %>% select(date,Missing) %>% arrange(desc(Missing))
G6.3<-G6.3[which(G6.3$Missing!=0),]
G6.3$Weekday<-wday(G6.3$date,label=TRUE)
G6.4<-activity
G6.4$weekday<-wday(G6.4$date,label=TRUE)
#Finding the mean of steps every monday and interval
G6.5<-aggregate(data=G6.4,steps~interval+weekday,FUN="mean",na.rm=TRUE)
#Merge the pre-imputation table G6.4 table with the average
G6.6<-merge(x=G6.4,y=G6.5,by.x=c("interval","weekday"),by.y=c("interval","weekday"),all.x=TRUE)
#Conditionally replacing the steps.x NA value with the values from steps.y value 
G6.6$Steps.Updated<-0
for (i in 1:dim(G6.6)[[1]]){
if(is.na(G6.6[i,3])){G6.6[i,6]=G6.6[i,5]}
else {G6.6[i,6]=G6.6[i,3]}
}
#Now simplify the imputed data frame
G6.6 <-G6.6  %>% select(date,weekday,interval,Steps.Updated)
names(G6.6)[[4]]<-"Steps"
```


## Step 7
Histogram of the total number of steps taken each day after missing values are imputed

```{r, echo = TRUE}
png("plot7.png")
qplot(G6.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
dev.off()
qplot(G6.6$Steps,geom="histogram",main="Total steps taken histogram post imputation",xlab="Steps",ylab="Count")
```




## Step 8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r, echo = TRUE}
G8<-G6.6
levels(G8$weekday)<-c(1,2,3,4,5,6,7)
G8$WDWE<-G8$weekday %in% c(1,2,3,4,5)
G8.1<-aggregate(data=G8,Steps~interval+WDWE,mean,na.rm=TRUE)
G8.1$WDWE<-as.factor(G8.1$WDWE)
levels(G8.1$WDWE)<-c("Weekend","Weekday")
png("plot8.png")
ggplot(data=G8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
dev.off()
ggplot(data=G8.1,aes(y=Steps,x=interval,group=1,color=WDWE))+geom_line() +scale_x_discrete(breaks = seq(0, 2500, by = 300))+ylab("Mean Steps")+xlab("Intervals")+ggtitle("Mean steps across intervals by Weekend and Weekday")
#Produce panel plot
G8.1$interval<-as.numeric(as.character(G8.1$interval))
library(lattice)
xyplot(data=G8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
library(hexbin)
hexbinplot(data=G8.1,Steps~interval|WDWE, aspect = 1, bins=50)
png("plott8.1.png")
xyplot(data=G8.1,Steps~interval|WDWE, grid = TRUE, type = c("p", "smooth"), lwd = 4,panel = panel.smoothScatter)
dev.off()
png("plot8.2.png")
hexbinplot(data=G8.1,Steps~interval|WDWE, aspect = 1, bins=50)
dev.off()
```
