

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



set.seed(5)
sample(1:8,size=4,replace=FALSE)




probs = c(5,5,5,5,1,1,1,1)/24
sample(1:8,size=4,replace=FALSE,prob=probs)



treat1 = sample(1:8,size=2,replace=FALSE); treat2 = sample(2:7,size=2,replace=FALSE)
c(treat1,treat2)



set.seed(5)
sample(1:8,size=4,replace=FALSE)



sample(c(1,3,4,7),size=2,replace=FALSE)


