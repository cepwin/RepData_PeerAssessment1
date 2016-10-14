q1<-function(){
x<-read.csv("activity.csv")

##remove N
x2<-as.data.frame(x[complete.cases(x),])
dates<-unique(as.Date(x2$date))
days<-split(x2,as.factor(x2$date))
dailySteps<-as.numeric(lapply(days,doSum))
dailySteps<-dailySteps[dailySteps>0]
hist(dailySteps)
print(summary(dailySteps))
}

##do lapply with a function that does the some of the data frame
doSum<-function(day) {
  df<-as.data.frame(day)
  sum(df[,1])
}

