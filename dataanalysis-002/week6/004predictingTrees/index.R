

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



data(iris)
names(iris)
table(iris$Species)



plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)



# An alternative is library(rpart)
library(tree)
tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)



plot(tree1)
text(tree1)



plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)



set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width = runif(20,2,4.5))
pred1 <- predict(tree1,newdata)
pred1



pred1 <- predict(tree1,newdata,type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)



data(Cars93,package="MASS")
head(Cars93)



treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + 
                   EngineSize + Width + Length + Weight + Price + Cylinders + 
                   Horsepower + Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)



par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))
pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)



pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)



table(Cars93$DriveTrain,predict(pruneTree,type="class"))
table(Cars93$DriveTrain,predict(treeCars,type="class"))


