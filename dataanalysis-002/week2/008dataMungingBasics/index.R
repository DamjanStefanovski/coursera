

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



fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.csv",method="curl")



cameraData <- read.csv("./data/cameras.csv")
names(cameraData)
tolower(names(cameraData))



splitNames = strsplit(names(cameraData),"\\.")
splitNames[[5]]
splitNames[[6]]



splitNames[[6]][1]
firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)



fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropbox.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv",method="curl")
download.file(fileUrl2,destfile="./data/solutions.csv",method="curl")
reviews <- read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews,2)
head(solutions,2)



names(reviews)
sub("_","",names(reviews),)




testName <- "this_is_a_test"
sub("_","",testName)
gsub("_","",testName)



reviews$time_left[1:10]
timeRanges <- cut(reviews$time_left,seq(0,3600,by=600))
timeRanges[1:10]



class(timeRanges)
table(timeRanges,useNA="ifany")



library(Hmisc)
timeRanges<- cut2(reviews$time_left,g=6)
table(timeRanges,useNA="ifany")



timeRanges<- cut2(reviews$time_left,g=6)
reviews$timeRanges <- timeRanges
head(reviews,2)



names(reviews)
names(solutions)



mergedData <- merge(reviews,solutions,all=TRUE)
head(mergedData)



mergedData2 <- merge(reviews,solutions,by.x="solution_id",by.y="id",all=TRUE)
head(mergedData2[,1:6],3)
reviews[1,1:6]



mergedData2$reviewer_id[1:10]
sort(mergedData2$reviewer_id)[1:10]



mergedData2$reviewer_id[1:10]
order(mergedData2$reviewer_id)[1:10]
mergedData2$reviewer_id[order(mergedData2$reviewer_id)]



head(mergedData2[,1:6],3)
sortedData <- mergedData2[order(mergedData2$reviewer_id),]
head(sortedData[,1:6],3)



head(mergedData2[,1:6],3)
sortedData <- mergedData2[order(mergedData2$reviewer_id,mergedData2$id),]
head(sortedData[,1:6],3)



misShaped <- as.data.frame(matrix(c(NA,5,1,4,2,3),byrow=TRUE,nrow=3))
names(misShaped) <- c("treatmentA","treatmentB")
misShaped$people <- c("John","Jane","Mary")
misShaped



melt(misShaped,id.vars="people",variable.name="treatment",value.name="value")


