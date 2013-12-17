

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache = T, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



download.file("https://dl.dropbox.com/u/7710864/data/csv_hid/ss06pid.csv",destfile="./data/ss06pid.csv",method="curl")



pData <- read.csv("./data/ss06pid.csv")



plot(pData$JWMNP,pData$WAGP,pch=19,col="blue")



plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)



plot(pData$JWMNP,pData$WAGP,pch=19,col=pData$SEX,cex=0.5)



percentMaxAge <- pData$AGEP/max(pData$AGEP)
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=percentMaxAge*0.5)



plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)
lines(rep(100,dim(pData)[1]),pData$WAGP,col="grey",lwd=5)
points(seq(0,200,length=100),seq(0,20e5,length=100),col="red",pch=19)



library(Hmisc)
ageGroups <- cut2(pData$AGEP,g=5)
plot(pData$JWMNP,pData$WAGP,pch=19,col=ageGroups,cex=0.5)



x <- rnorm(1e5)
y <- rnorm(1e5)
plot(x,y,pch=19)



x <- rnorm(1e5)
y <- rnorm(1e5)
sampledValues <- sample(1:1e5,size=1000,replace=FALSE)
plot(x[sampledValues],y[sampledValues],pch=19)



x <- rnorm(1e5)
y <- rnorm(1e5)
smoothScatter(x,y)



library(hexbin)
x <- rnorm(1e5)
y <- rnorm(1e5)
hbo <- hexbin(x,y)
plot(hbo)



x <- rnorm(20); y <- rnorm(20)
qqplot(x,y)
abline(c(0,1))



X <- matrix(rnorm(20*5),nrow=20)
matplot(X,type="b")



image(1:10,161:236,as.matrix(pData[1:10,161:236]))



newMatrix <- as.matrix(pData[1:10,161:236])
newMatrix <- t(newMatrix)[,nrow(newMatrix):1]
image(161:236, 1:10, newMatrix)



library(maps)
map("world")
lat <- runif(40,-180,180); lon <- runif(40,-90,90)
points(lat,lon,col="blue",pch=19)




x <- c(NA,NA,NA,4,5,6,7,8,9,10)
y <- 1:10
plot(x,y,pch=19,xlim=c(0,11),ylim=c(0,11))



x <- rnorm(100)
y <- rnorm(100)
y[x < 0] <- NA
boxplot(x ~ is.na(y))


