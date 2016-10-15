question2<-function(){
require(tidyr)
activity<-read.csv("activity.csv")

##remove N
x2<-as.data.frame(activity[complete.cases(activity),])
x2$steps<-as.integer(x2$steps)
dates<-unique(as.Date(x2$date))
x4<-spread(x2,interval,steps)

sumcol<-subset(x4, select = -date)
cmeans<-colMeans(sumcol)

replace<-which(is.na(activity$steps))

print(length(replace))
## for each item in x if is.na(Steps) replace with
## replacement[replacement$ctimes = x[i,x$interval],replacement$cmeans]
for(i in replace) {
    row<-i %% 288 + 1
    rep<-as.integer(cmeans[row])
    activity[i,1]<- rep
}
activity
dates<-unique(as.Date(activity$date))
days<-split(activity,as.factor(activity$date))
dailySteps<-as.numeric(lapply(days,doSum))
dailySteps<-dailySteps[dailySteps>0]
hist(dailySteps)
print(summary(dailySteps))

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
weVal2<-subset(weVal,select = -dows)
we2<-spread(weVal2,interval,steps)

sumcol<-subset(we2, select = -date)
cmeans<-colMeans(sumcol)
ctimes<-names(we2)
ctimes<-ctimes[2:289]


##do the same for the weekday data
sumcol<-subset(wd2, select = -date)
cmeansWD<-colMeans(sumcol)
ctimesWD<-names(wd2)
ctimesWD<-ctimes[2:289]



}


##do lapply with a function that does the some of the data frame
doSum<-function(day) {
  df<-as.data.frame(day)
  sum(df[,1])
}

