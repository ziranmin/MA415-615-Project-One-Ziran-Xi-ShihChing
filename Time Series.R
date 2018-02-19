
ts1 <- read.csv("PB monthly summaries.csv")
#delete first column
ts2 <- ts1[2:13]
#rearrange the columns by moving Oct, Nov, and Dec to the end 
ts3 <- cbind(subset.data.frame(ts2)[,4:12],subset.data.frame(ts2)[,1:3])
#reverse data 
ts4 <- ts3[ nrow(ts3):1, ]
#turn it to vectors 
ts5 <- as.vector(t(ts4))
#create time series 
ts6 <- ts(ts5,frequency=12,start=c(2000,1))
#plot time series
ts.plot(ts6, gpars=list(main ="Time Series Chart of Apprehensions from 2000 to 2017",xlab="year", ylab="Apprehensions", col= "black", lty=c(1:3)))
#caculate annual averages
ts_avg <- apply(ts4,1,mean)
#create annual averages in time series
ts_avg<-ts(ts_avg,start = c(2000.5), frequency=1)

#create segments and text for annual averages
for (i in 1:18) {
  segments(1999+i,ts_avg[i],2000+i,ts_avg[i],col = 2,lwd = 1)
  text(2000.3 + i, ts_avg[i], labels = 1999+i, cex=0.6, col = "red" , font=1)
}
#create points and connected dashed line for better visualization
points(ts_avg,col = "blue", pch=20)
lines(ts_avg,col = "blue", pch=20,lty=3)
#create legends
legend(2010,220000, legend = c("Monthly Apprehensions","Annual Averages by segments","Annual Averages by Points","Annual Average Connected by Dashed Line"), 
       col = c("black","red","blue", "blue"),pch =c(NA,NA,20,NA),lty=c(1,1,NA,3), cex=0.75)



