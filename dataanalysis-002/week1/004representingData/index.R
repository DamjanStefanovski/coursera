

# make this an external chunk that can be included in any file
options(width = 70)
opts_chunk$set(message = F, error = F, warning = F, echo = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache = T, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



xvals = 0:10
plot(xvals,dbinom(xvals,10,0.5),type="h",col="blue",lwd=3,ylab="probability",xlab="X value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,0,1),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




xvals = seq(-2,2,length=100)
plot(xvals,dunif(xvals,0,1),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,0,1),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,0,5),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,5,1),type="l",col="blue",lwd=3,ylab="density",xlab="X value")




xvals = 0:10
plot(xvals,dbinom(xvals,10,0.5),type="h",col="blue",lwd=3,ylab="probability",xlab="X value")




xvals = 0:10
plot(xvals,dbinom(xvals,10,0.8),type="h",col="blue",lwd=3,ylab="probability",xlab="X value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,5,1),type="l",col="blue",lwd=3,ylab="density",xlab="Y value")




xvals = seq(-10,10,length=100)
plot(xvals,dnorm(xvals,0,2),type="l",col="blue",lwd=3,ylab="density",xlab="X value")



