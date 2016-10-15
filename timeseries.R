timeseries<-function() {
require(tidyr)
x<-read.csv("activity.csv")

##remove N
x2<-as.data.frame(x[complete.cases(x),])
dates<-unique(as.Date(x2$date))
x4<-spread(x2,interval,steps)

sumcol<-subset(x4, select = -date)
cmeans<-colMeans(sumcol)
ctimes<-names(x4)
print(ctimes)
ctimes<-ctimes[2:289]
plot(x=ctimes,y=cmeans,type="l",xlab = "Time (in 5 min intervals",ylab = "Mean Steps")

}
