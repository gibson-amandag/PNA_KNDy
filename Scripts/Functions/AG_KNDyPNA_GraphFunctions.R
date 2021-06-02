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

plotBurstParamBox_overTime <- function(
  param,
  param_overTime, #df
  niceName = KNDy_VarNames
){
  param_long <- make_20minBins_long_forBox(param_overTime)
  param_long <- add_Bin_col_forBox(param_long)
  viz <- ggboxplot(
    param_long,
    x = "GenTreatment",
    y = "paramValue",
    color = "Time",
    palette = "jco",
    facet.by = "AgeGroup",
    short.panel.labs = TRUE,
    add = c("jitter")
  ) +
    ylab(niceName[, as.character(param)]) + 
    xlab("Treatment")
  return(viz)
}

plotBurstParamBox <- function(
  df,
  param,
  niceNames = KNDy_VarNames
){
  viz <- ggboxplot(
    df,
    x = "GenTreatment",
    y = as.character(param),
    facet.by = "AgeGroup",
    add = c("jitter"),
    color = "GenTreatment",
    palette = "jco"
  ) + ylab(niceNames[,as.character(param)]) +
    xlab("Treatment")+
    guides(color = guide_legend("Treatment"))
  
  return(viz)
}