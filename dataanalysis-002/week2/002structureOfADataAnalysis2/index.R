

# make this an external chunk that can be included in any file
options(width = 100)
opts_chunk$set(message = F, error = F, warning = F, comment = NA, fig.align = 'center', dpi = 100, tidy = F, cache = F, cache.path = '.cache/', fig.path = 'fig/')

options(xtable.type = 'html')
knit_hooks$set(inline = function(x) {
  if(is.numeric(x)) {
    round(x, getOption('digits'))
  } else {
    paste(as.character(x), collapse = ', ')
  }
})
knit_hooks$set(plot = knitr:::hook_plot_html)



# If it isn't installed, install the kernlab package
library(kernlab)
data(spam)
# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601,size=1,prob=0.5)
table(trainIndicator)
trainSpam = spam[trainIndicator==1,]
testSpam = spam[trainIndicator==0,]



names(trainSpam)



head(trainSpam)



table(trainSpam$type)



plot(trainSpam$capitalAve ~ trainSpam$type)



plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)



plot(log10(trainSpam[,1:4]+1))



par(mar=c(0,0,0,0))




hCluster = hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)



hClusterUpdated = hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)



trainSpam$numType = as.numeric(trainSpam$type)-1
costFunction = function(x,y){sum(x!=(y > 0.5))}
cvError = rep(NA,55)
library(boot)
for(i in 1:55){
  lmFormula = as.formula(paste("numType~",names(trainSpam)[i],sep=""))
  glmFit = glm(lmFormula,family="binomial",data=trainSpam)
  cvError[i] = cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
which.min(cvError)
names(trainSpam)[which.min(cvError)]




predictionModel = glm(numType ~ charDollar,family="binomial",data=trainSpam)
predictionTest = predict(predictionModel,testSpam)
predictedSpam = rep("nonspam",dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] = "spam"
table(predictedSpam,testSpam$type)
(61+458)/(1346+458 + 61 + 449)



