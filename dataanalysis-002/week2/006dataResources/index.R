

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


