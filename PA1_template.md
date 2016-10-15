# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
activity<-read.csv("activity.csv")

##remove N
activity_cc<-as.data.frame(activity[complete.cases(activity),])
dates<-unique(as.Date(activity_cc$date))
days<-split(activity_cc,as.factor(activity_cc$date))
```

## What is mean total number of steps taken per day?

```r
##do lapply with a function that does the some of the data frame
doSum<-function(day) {
  df<-as.data.frame(day)
  sum(df[,1])
}

##breaking the data into days
dates<-unique(as.Date(activity_cc$date))
days<-split(activity_cc,as.factor(activity_cc$date))

##creating the daily total for the steps
dailySteps<-as.numeric(lapply(days,doSum))
dailySteps<-dailySteps[dailySteps>0]  ##ignoring days where there are no steps as first and last day is NA
##Histogram of total daily steps
hist(dailySteps)
```

![](PA1_template_files/figure-html/numSteps-1.png)<!-- -->

```r
##summary including the mean and median
summary(dailySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```



## What is the average daily activity pattern?

I will spread the complete data set so that each interval is a column (fat data.)  I will then create the mean of the columns
to obtain the mean steps per interval.  I will get the time intervals from the names of this dataset and then plot the means vs the times


```r
require(tidyr)
```

```
## Loading required package: tidyr
```

```r
activity_int<-spread(activity_cc,interval,steps)

sumcol<-subset(activity_int, select = -date)
cmeans<-colMeans(sumcol)
ctimes<-names(activity_int)
ctimes<-ctimes[2:289]
plot(x=ctimes,y=cmeans,type="l",xlab = "Time (in 5 min intervals",ylab = "Mean Steps", main="Daily Activity Pattern")
```

![](PA1_template_files/figure-html/dailyActivity-1.png)<!-- -->



## Imputing missing values
NOTE: My method for creating the missing values is taking the average for each interval using the dataset with only complete clases (x2
)  I then get a list of incomplete rows and for each empty value I replace it with the mean value for that 5 minute interval.
Calculate the rows that need to be replaced

```r
replace<-which(is.na(activity$steps))
```

Calculate the number of rows by getting the length

```r
length(replace)
```

```
## [1] 2304
```

Do the replacement

```r
## for each item in x if is.na(Steps) replace with
## cmeans[row]
for(i in replace) {
    row<-i %% 288 + 1  ##each day has 288 intervals so I figure out what interval it is using the modulo of 288 +1 (since its base 1)
    rep<-as.integer(cmeans[row])
    activity[i,1]<- rep
}
```

Create the histogram of data with NA's replaced

```r
dates<-unique(as.Date(activity$date))
days<-split(activity,as.factor(activity$date))
dailySteps<-as.numeric(lapply(days,doSum))
dailySteps<-dailySteps[dailySteps>0]
hist(dailySteps)
```

![](PA1_template_files/figure-html/dailySteps2-1.png)<!-- -->

The summary of daily steps where NA is replaced


```r
summary(dailySteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10640   10750   12810   21190
```


## Are there differences in activity patterns between weekdays and weekends?

Process the updated data (where NA is replace) to create separate datasets for weekends and weekdays and create timeseries graphs

```r
##create the weekday and weekend column
dows<-weekdays(as.Date(activity$date))
dows[dows=="Sunday" | dows=="Saturday"]<-"Weekend"
dows[!(dows=="Weekend")]<-"Weekday"

##bind it to the updated data and then splt using the weekend/weekday column as a factor
x_wd<-cbind(activity,dows)
days2<-split(x_wd,as.factor(x_wd$dows))
wdVal<-days2[[1]]
weVal<-days2[[2]]

##spread the data and calculate the means as was done earlier
wdVal2<-subset(wdVal,select = -dows)
wd2<-spread(wdVal2,interval,steps)

sumcol<-subset(wd2, select = -date)
cmeans<-colMeans(sumcol)
ctimes<-names(wd2)
ctimes<-ctimes[2:289]

##setup and create the first plot
par(mfcol=c(2,1))
par(mar=c( 2.1, 4.1, 2.1, 2.1))
plot(x=ctimes,y=cmeans,type="l",xlab = "Time (in 5 min intervals",ylab = "Mean Steps", main = "Weekday Steps")

##do the same for the weekend data
weVal2<-subset(weVal,select = -dows)
we2<-spread(weVal2,interval,steps)

sumcol<-subset(we2, select = -date)
cmeans<-colMeans(sumcol)
ctimes<-names(we2)
ctimes<-ctimes[2:289]
plot(x=ctimes,y=cmeans,type="l",xlab = "Time (in 5 min intervals",ylab = "Mean Steps", main = "Weekend Steps")
```

![](PA1_template_files/figure-html/activityCompare-1.png)<!-- -->