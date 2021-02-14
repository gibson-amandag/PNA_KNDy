#Load my theme
my_theme = theme(
  text = element_text(size=16),
  legend.title = element_blank(),
  legend.position = "bottom",
  panel.background = element_rect(fill = "transparent"), # bg of the panel
  plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
  panel.grid.major = element_blank(), # get rid of major grid
  panel.grid.minor = element_blank(), # get rid of minor grid
  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
  legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
  axis.line = element_line(colour = "black")
) 

#scale_color_manual(values = c("Amanda" = "orange", "Jenn" = "blue"))
