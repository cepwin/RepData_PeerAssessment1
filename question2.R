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

## for each item in x if is.na(Steps) replace with
## replacement[replacement$ctimes = x[i,x$interval],replacement$cmeans]
for(i in replace) {
##  if(is.na(x[i,1])) {
    row<-i %% 288 + 1
    rep<-as.integer(cmeans[row])
    x[i,1]<- rep
##  }
}
x
}