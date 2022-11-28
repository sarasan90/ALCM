# create empty theme to clear the plot area
empty_theme <- theme(                              
  plot.background = element_blank(), 
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank(), 
  panel.background = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_text(angle = 90)
)