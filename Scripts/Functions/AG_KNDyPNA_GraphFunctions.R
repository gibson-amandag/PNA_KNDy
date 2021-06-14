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

# param as character
AG_KNDyPNA_manuscriptPlot <- function(df, param){
  viz <- ggerrorplot(
    df,
    "GenTreatment",
    param,
    add = "jitter",
    palette = c("white", "black"),
    facet.by = "AgeGroup",
    size = .4,
    add.params = list(shape = 21, fill = "GenTreatment", size = 1, alpha = .8),
    xlab = KNDy_VarNames[,"GenTreatment"],
    ylab = KNDy_VarNames[, param],
    error.plot = "linerange",
    font.x = c(size = 11, style = "bold"),
    font.y = c(size = 11, style = "bold"),
    font.family = "Arial",
    font.tickslab = c(size = 11 ),
    font.label = c(size = 11),
    panel.labs.font = list(size = 11, face = "bold")
  ) +
    stat_summary(
      geom = "errorbar", 
      fun.min = mean, 
      fun = mean, 
      fun.max = mean, 
      width = .3,
      size = .4
    ) +
    stat_summary(
      geom = "errorbar", 
      fun.min = median, 
      fun = median, 
      fun.max = median, 
      width = .3,
      size = .4,
      color = "red",
      alpha = .7
    ) +
    rremove(
      "legend"
    ) +
    theme(
      element_blank()
    )
  
  return(viz)
}
