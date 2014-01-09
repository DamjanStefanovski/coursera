

# make this an external chunk that can be included in any file
options(width = 70)
opts_chunk$set(message = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache = T, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt"
download.file(fileUrl,destfile="./data/earthquakeData.csv",method="curl")
dateDownloaded <- date()
dateDownloaded
eData <- read.csv("./data/earthquakeData.csv")



eData



dim(eData)
names(eData)
nrow(eData)



quantile(eData$Lat)
summary(eData)



class(eData)
sapply(eData[1,],class)



unique(eData$Src)
length(unique(eData$Src))
table(eData$Src)



table(eData$Src,eData$Version)



eData$Lat[1:10]
eData$Lat[1:10] > 40
any(eData$Lat[1:10] > 40)




eData$Lat[1:10] > 40
all(eData$Lat[1:10] > 40)



eData[eData$Lat > 0 & eData$Lon > 0,c("Lat","Lon")]




eData[eData$Lat > 0 | eData$Lon > 0,c("Lat","Lon")]




fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")
reviews <- read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews,2)
head(solutions,2)



is.na(reviews$time_left[1:10])
sum(is.na(reviews$time_left))
table(is.na(reviews$time_left))



table(c(0,1,2,3,NA,3,3,2,2,3))
table(c(0,1,2,3,NA,3,3,2,2,3),useNA="ifany")



colSums(reviews)



colMeans(reviews,na.rm=TRUE)
rowMeans(reviews,na.rm=TRUE)


