

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



download.file("http://www.rossmanchance.com/iscam2/data/movies03RT.txt",
              destfile="./data/movies.txt")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)



aovObject <- aov(movies$score ~ movies$rating)
aovObject



aovObject$coeff



aovObject2 <- aov(movies$score ~ movies$rating + movies$genre)
aovObject2



summary(aovObject2)



aovObject3 <- aov(movies$score ~ movies$genre + movies$rating)
summary(aovObject3)
summary(aovObject2)



aovObject4 <- aov(movies$score ~ movies$genre + movies$rating + movies$box.office)
summary(aovObject4)


