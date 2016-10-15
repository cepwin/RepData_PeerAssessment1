question2<-function(){
require(tidyr)
x<-read.csv("activity.csv")

##remove N
x2<-as.data.frame(x[complete.cases(x),])
x2$steps<-as.integer(x2$steps)
dates<-unique(as.Date(x2$date))
x4<-spread(x2,interval,steps)

sumcol<-subset(x4, select = -date)
cmeans<-colMeans(sumcol)

replace<-which(is.na(x$steps))

print(length(replace))
## for each item in x if is.na(Steps) replace with
## replacement[replacement$ctimes = x[i,x$interval],replacement$cmeans]
for(i in replace) {
    row<-i %% 288 + 1
    rep<-as.integer(cmeans[row])
    x[i,1]<- rep
}
x
dates<-unique(as.Date(x$date))
days<-split(x,as.factor(x$date))
dailySteps<-as.numeric(lapply(days,doSum))
dailySteps<-dailySteps[dailySteps>0]
hist(dailySteps)
print(summary(dailySteps))

dows<-weekdays(as.Date(x$date))
dows[dows=="Sunday" | dows=="Saturday"]<-"Weekend"
dows[!(dows=="Weekend")]<-"Weekday"
x_wd<-cbind(x,dows)
days2<-split(x_wd,as.factor(x_wd$dows))
dailySteps2<-as.numeric(lapply(days2,doSum))
dailySteps2<-dailySteps2[dailySteps2>0]
hist(dailySteps2)

}


##do lapply with a function that does the some of the data frame
doSum<-function(day) {
  df<-as.data.frame(day)
  sum(df[,1])
}

