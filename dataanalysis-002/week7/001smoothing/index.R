

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



download.file("https://spark-public.s3.amazonaws.com/dataanalysis/cd4.data",
              destfile="./data/cd4.data",method="curl")
cd4Data <- read.table("./data/cd4.data", 
                      col.names=c("time", "cd4", "age", "packs", "drugs", "sex",
                                  "cesd", "id"))
cd4Data <- cd4Data[order(cd4Data$time),]
head(cd4Data)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(mean(cd4Data$time[1:2]),mean(cd4Data$cd4[1:2]),col="blue",pch=19)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(mean(cd4Data$time[1:2]),mean(cd4Data$cd4[1:2]),col="blue",pch=19)
points(mean(cd4Data$time[2:3]),mean(cd4Data$cd4[2:3]),col="blue",pch=19)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(3:(dim(cd4Data)[1]-2)))
for(i in 3:(dim(cd4Data)[1]-2)){
    aveTime[i] <- mean(cd4Data$time[(i-2):(i+2)])
    aveCd4[i] <- mean(cd4Data$cd4[(i-2):(i+2)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(11:(dim(cd4Data)[1]-10)))
for(i in 11:(dim(cd4Data)[1]-2)){
  aveTime[i] <- mean(cd4Data$time[(i-10):(i+10)])
 aveCd4[i] <- mean(cd4Data$cd4[(i-10):(i+10)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
aveTime <- aveCd4 <- rep(NA,length(201:(dim(cd4Data)[1]-200)))
for(i in 201:(dim(cd4Data)[1]-2)){
    aveTime[i] <- mean(cd4Data$time[(i-200):(i+200)])
    aveCd4[i] <- mean(cd4Data$cd4[(i-200):(i+200)])
}
lines(aveTime,aveCd4,col="blue",lwd=3)



filtTime <- as.vector(filter(cd4Data$time,filter=rep(1,200))/200)
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,200))/200)
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)



filtCd4 <- as.vector(filter(cd4Data$cd4,filter=rep(1,4))/4)
filtCd4[2]
sum(cd4Data$cd4[1:4] * rep(1/4,4))



ws = 10; tukey = function(x) pmax(1 - x^2,0)^2
filt= tukey(seq(-ws,ws)/(ws+1));filt=filt/sum(filt)
plot(seq(-(ws),(ws)),filt,pch=19)



ws = 100; tukey = function(x) pmax(1 - x^2,0)^2
filt= tukey(seq(-ws,ws)/(ws+1));filt=filt/sum(filt)
filtTime <- as.vector(filter(cd4Data$time,filter=filt))
filtCd4 <- as.vector(filter(cd4Data$cd4,filter=filt))
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1); lines(filtTime,filtCd4,col="blue",lwd=3)



lw1 <- loess(cd4 ~ time,data=cd4Data)
plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
lines(cd4Data$time,lw1$fitted,col="blue",lwd=3)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1,ylim=c(500,1500))
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.1)$fitted,col="blue",lwd=3)
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.25)$fitted,col="red",lwd=3)
lines(cd4Data$time,loess(cd4 ~ time,data=cd4Data,span=0.76)$fitted,col="green",lwd=3)



tme <- seq(-2,5,length=100); pred1 = predict(lw1,newdata=data.frame(time=tme),se=TRUE)
plot(tme,pred1$fit,col="blue",lwd=3,type="l",ylim=c(0,2500))
lines(tme,pred1$fit + 1.96*pred1$se.fit,col="red",lwd=3)
lines(tme,pred1$fit - 1.96*pred1$se.fit,col="red",lwd=3)
points(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)



library(splines)
ns1 <- ns(cd4Data$time,df=3)
par(mfrow=c(1,3))
plot(cd4Data$time,ns1[,1]); plot(cd4Data$time,ns1[,2]); plot(cd4Data$time,ns1[,3])



lm1 <- lm(cd4Data$cd4 ~ ns1)
summary(lm1)



plot(cd4Data$time,cd4Data$cd4,pch=19,cex=0.1)
points(cd4Data$time,lm1$fitted,col="blue",pch=19,cex=0.5)


