

# make this an external chunk that can be included in any file
options(width = 70)
opts_chunk$set(message = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache=F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



getwd()
setwd("/Users/jtleek/Dropbox/Jeff/teaching/2013/coursera/week2/004gettingData1/data")
getwd()



##  setwd("C:\\Users\\Andrew\\Downloads")



getwd()
setwd("./data")
getwd()
setwd("../")
getwd()



getwd()
setwd("/Users/jtleek/Dropbox/Jeff/teaching/2013/coursera/week2/004gettingData1/data")
getwd()



fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.csv",method="curl")
list.files("./data")
dateDownloaded <- date()
dateDownloaded



getwd()
cameraData <- read.table("./data/cameras.csv")
head(cameraData)




getwd()
cameraData <- read.table("./data/cameras.csv",sep=",",header=TRUE)
head(cameraData)




cameraData <- read.csv("./data/cameras.csv")
head(cameraData)



library(xlsx)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/camera.xlsx",method="curl")
cameraData <- read.xlsx2("./data/camera.xlsx",sheetIndex=1)
head(cameraData)



## cameraData <- read.csv(file.choose())


