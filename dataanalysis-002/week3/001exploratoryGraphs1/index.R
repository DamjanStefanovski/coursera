

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



boxplot(pData$AGEP,col="blue")



boxplot(pData$AGEP ~ as.factor(pData$DDRS),col="blue")



boxplot(pData$AGEP ~ as.factor(pData$DDRS),col=c("blue","orange"),names=c("yes","no"),varwidth=TRUE)



barplot(table(pData$CIT),col="blue")



hist(pData$AGEP,col="blue")



hist(pData$AGEP,col="blue",breaks=100,main="Age")



dens <- density(pData$AGEP)
plot(dens,lwd=3,col="blue")



dens <- density(pData$AGEP)
densMales <- density(pData$AGEP[which(pData$SEX==1)])
plot(dens,lwd=3,col="blue")
lines(densMales,lwd=3,col="orange")


