---
title: "PA1_template"
author: "Helen"
date: "Friday, August 08, 2014"
output: html_document
---

### Project introduction

This project uses data from activity monitoring device of a single anonymous individual within October and November, 2012. The device collects data at 5 minute intervals through out the day, and includes the number of steps taken in 5 minute intervals each day.

**Variables** included in this dataset are:

1. *steps* : Number of steps taking in a 5-minute interval (missing values are coded as NA)

2. *date* : The date on which the measurement was taken in YYYY-MM-DD format

3.*interval* : Identifier for the 5-minute interval in which measurement was taken

**^** *Special notes here : steps contains missing values (NA) and are treated differently in below sections. Please refer to information and comments from each individual section.*

Further details on project requirements and data source acknoledgement, please refer to [*Github page BY Roger D. Peng*](https://github.com/rdpeng/RepData_PeerAssessment1)

### 1. Loading and preprocess on raw data
This step performs the first phase preparation, including:

1.1 set working directory

1.2 download data file

1.3 read downloaded .csv file into a data frame
```{r}
raw<-read.csv(".\\activity.csv")
```

1.4 apply format on "date" column for later manupilation
```{r}
raw$date<-as.Date(raw$date)
```

1.5 load required package

Packages used in this project are **plyr**,**ggplot2**, and **lattice**. In coding, packages can be loaded up front. However for this documents,package calls actually happen when it's required from each section.


### 2. Total number of steps taken per day
This section is required to ignore NA values on steps. ```{r}ddply``` function is used on imported dataset with NA removed to get a total number of steps by date.
```{r}
require(plyr)
total_steps<-ddply(raw[!is.na(raw$steps),], c("date"), summarize, steps = sum(steps))
```
A histogram plot can be made from returned data frame ```{r}total_steps``` using base graphic function ```{r} hist(total_steps$steps,...) ``` which looks like below:

```{r,echo=FALSE}
hist(total_steps$steps,breaks=20,
     main="Distribution of Total Steps (by per day)",
     xlab="x = Total steps per day",
     cex.axis=0.8,cex.main=0.9,cex.lab=0.9,col="skyblue1")
```

Continue working from the same dataset ```{r}total_steps```, mean of steps equals 
```{r,echo=FALSE}
mean(total_steps$steps)
```
and median of steps equals
```{r,echo=FALSE}
median(total_steps$steps)
```


### 3. Average daily activity pattern
This section focuses on average pattern of steps in relation to 5-minute intervals for entire date range. Similar to above analysis,```{r}ddply``` function is used, but instead of summarizing the total steps by date, now summarization takes the average by intervals across all dates where NA values are removed by arguments within ```{r} mean()``` operation.

```{r}
avg_int_steps<-ddply(raw, c("interval"), summarize, steps = mean(steps,na.rm=TRUE))
```

A time series plot of avg(steps) can be drawn by base graphic function ```{r} plot(x,y,...)``` where x is the interval measurements, and y is avg(steps) on each interval over entire observation date range (two months period). Annovations to the basic look i.e.aesthetic and geometric preferences can be added as "..." arguments.

```{r,echo=FALSE}
plot(avg_int_steps$interval,avg_int_steps$steps,
     main="Pattern of Average Steps (over 2 months)",
     xlab="x = interval",ylab="y = avg(steps)",
     cex.lab=0.9,cex.main=0.9,cex.axis=0.8,
     type="l",lwd=2,col="hotpink2")
```

It's not easy to measure statistics i.e.max/min from reading plots as above without extra annovation efforts, however below codes are quite handy, for example, to get max of Avg(steps):
```{r}
avg_int_steps[which.max(avg_int_steps$steps),]
```

This clearly shows that the maximum of average steps by intervals happen at interval 835, and the average steps for this interval is 206.2 over 2 months observation.


### 4. Imputing Missing Values
This section aims at an approach to handle NA values by compensating or minimizing its impacts as well as make a more complete dataset.

Firstly, to understand how many measurements are experiencing NA values, a simple row count on the ```{r} is.na()``` subset of original dataset (recall as ```{r} raw```) returns:
```{r,echo=FALSE}
nrow(raw[is.na(raw$steps),])
```

Secondly, to determine an approach to "recover" missing values, it's been realized that some dates have all measurements from every intervals missing. Hence I decide to go with the average from overall period and feed it to NA values where it has the same intervals. This approach utilizes result from previous **STEP 3**.

To achieve this, a copy of original data is made and named as ```{r} DRV_raw``` . ```{r} for``` then looped through all intervals, and update missing values on derived data frame with average from *STEP 3* result. To be specific, it look like
```{r}
DRV_raw<-raw

for (i in avg_int_steps$interval) {
DRV_raw$steps[is.na(DRV_raw$steps)&DRV_raw$interval==i]<-avg_int_steps$steps[avg_int_steps$interval==i]     
}
```
This new derived dataset now doesn't contain any missing values. Again if similar transformations are applied with ```{r}ddply```
function call and the hist plotting *(refer to Step 2 from above)* now looks like below

```{r,echo=FALSE}
DRV_total_steps<-ddply(DRV_raw, c("date"), summarize, steps = sum(steps))

hist(DRV_total_steps$steps,breaks=20,
     main="Distribution of Total Steps (by per day)",
     xlab="x = Total steps per day",
     cex.axis=0.8,cex.main=0.9,cex.lab=0.9,col="skyblue1")
```

Again mean and median can be taken the same way. Mean is now
```{r,echo=FALSE}
mean(DRV_total_steps$steps)
```

and median is now
```{r,echo=FALSE}
median(DRV_total_steps$steps)
```

As it's hard to link the difference together without looking at the distribution side by side, function ```{r} qplot(x,data,facets=~.,...)``` from package **ggplot2** is quite powerful to put things like this together. A few transformations required to put derived and raw into one data frame, but separated by a facet (named [type] in my work), which produces below comparison look:
```{r,echo=FALSE}
require(ggplot2)
total_steps$type<-rep("raw",nrow(total_steps))
DRV_total_steps$type<-rep("derived",nrow(DRV_total_steps))
compare_total_steps<-rbind(total_steps,DRV_total_steps)
qplot(steps,data=compare_total_steps,
      facets=type~.,main="Derived vs Raw Comparison",xlab=" x = Total steps per day",cex.main=0.9,cex.labs=0.8)
```

To draw a conclusion to this approach, NA impact to overall median is eliminated but the distribution of total steps pattern is shifted comparing to the raw data with NAs. I think which approach to choose is more a matter of analytical direction and goal to achieve.


### 5. Weekday/Weekend Analysis
Based on above derived data, this section compares average steps by intervals among two audience group: weekend and weekday. To achieve such purposed, some data formation are done to:

- get weekday/weekend by ```{r} weekdays()``` function

- apply ```{r} ddply``` to get the average by interval & weekday/weekend type

- factor this type for later plotting 

```{r}
DRV_raw$weekday[weekdays(DRV_raw$date)=="Saturday"|weekdays(DRV_raw$date)=="Sunday"]<-"weekend"
DRV_raw$weekday[is.na(DRV_raw$weekday)]<-"weekday"

DRV_avg_int_steps<-ddply(DRV_raw, c("weekday","interval"), summarize, steps = mean(steps))
DRV_avg_int_steps<-transform(DRV_avg_int_steps,weekday=factor(weekday))
```

Once above transformation is completed, package **lattice** is required to produce pannels like below. Plotting function called is ```{r} xyplot(y~x|f,data,...)``` In this instance, dataset is ```{r} DRV_avg_int_steps ``` with y = steps, x = interval and f = weekday, extra aesthetic and geometric arguments to follow in "...".


```{r,echo=FALSE}
require(lattice)
xyplot(steps~interval|weekday,data=DRV_avg_int_steps,type="l",
       main="Weekend - Weekday Comparison",
       ylab="Number of steps",
       layout=c(1,2))
```

---
This is the end of this document.






