

# make this an external chunk that can be included in any file
options(width = 70)
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



args(rnorm)
heights = rnorm(10,mean=188,sd=3)
heights



args(rbinom)
coinFlips = rbinom(10,size=10,prob=0.5)
coinFlips



xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,0,1),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




args(dnorm)
x = seq(from=-5,to=5,length=10)
normalDensity = dnorm(x,mean=0,sd=1)
round(normalDensity,2)




xvals = 0:10
plot(xvals,dbinom(xvals,10,0.5),type="h",col="blue",lwd=3,ylab="probability",xlab="X value")




args(dbinom)
x = seq(0,10,by=1)
binomialDensity = dbinom(x,size=10,prob=0.5)
round(binomialDensity,2)



args(sample)
heights = rnorm(10,mean=188,sd=3)
heights
sample(heights,size=10,replace=TRUE)




heights
sample(heights,size=10,replace=FALSE)




heights
probs = c(0.4,0.3,0.2,0.1,0,0,0,0,0,0)
sum(probs)
sample(heights,size=10,replace=TRUE,prob=probs)




set.seed(12345)
rnorm(5,mean=0,sd=1)

set.seed(12345)
rnorm(5,mean=0,sd=1)


