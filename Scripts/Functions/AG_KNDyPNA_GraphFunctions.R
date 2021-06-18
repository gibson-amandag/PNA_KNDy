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
    # font.family = "Arial",
    font.tickslab = c(size = 11 ),
    font.label = c(size = 11),
    panel.labs.font = list(size = 11, face = "bold")
  ) +
    stat_summary(
      geom = "errorbar", 
      fun.min = mean, 
      fun = mean, 
      fun.max = mean, 
      width = .7,
      size = .4
    ) +
    stat_summary(
      geom = "errorbar", 
      fun.min = median, 
      fun = median, 
      fun.max = median, 
      width = .7,
      size = .4,
      color = "red",
      alpha = .7
    ) +
    expand_limits(y = 0) +
    rremove(
      "legend"
    ) +
    theme(
      text = element_text(family = "Arial")
    ) +
    rremove("xlab")
  
  return(viz)
}

AG_KNDyPNA_manuscriptPlot_AGD <- function(
  df,
  param,
  ylab,
  addSig = TRUE,
  yAdd = 0.5 # for bumping up significance
){
  viz <- ggerrorplot(
    df,
    "GenTreatment",
    param,
    add = "jitter",
    palette = c("white", "black"),
    size = .4,
    add.params = list(shape = 21, fill = "GenTreatment", size = 1, alpha = .8),
    xlab = KNDy_VarNames[,"GenTreatment"],
    ylab = ylab,
    error.plot = "linerange",
    font.x = c(size = 11, style = "bold"),
    font.y = c(size = 11, style = "bold"),
    font.tickslab = c(size = 11 ),
    font.label = c(size = 11),
    panel.labs.font = list(size = 11, face = "bold")
  ) +
    stat_summary(
      geom = "errorbar", 
      fun.min = mean, 
      fun = mean, 
      fun.max = mean, 
      width = .7,
      size = .4
    ) +
    expand_limits(y=0) +
    rremove(
      "legend"
    ) + 
    rremove(
      "xlab"
    )
  
  if(addSig){
    viz <- viz +
      stat_compare_means(
        method = "t.test", 
        label.y = max(df[, param], na.rm = TRUE) + yAdd,
        label = "p.signif", 
        ref.group = "CON"
      )
  }
  
  viz <- viz + theme(
    # panel.background = element_rect(fill = "transparent"), # bg of the panel
    panel.background = element_blank(), # bg of the panel
    # plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    plot.background = element_blank(), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    # legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.background = element_blank(), # get rid of legend bg
    # legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
    legend.box.background = element_blank(), # get rid of legend panel bg
    text = element_text(family = "Arial")
  )
  
  return(viz)
}

AG_KNDyPNA_baseSenkPlot <- function(
  df_long
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = firingRate
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = CellID),
      position = position_dodge(0.4)
    ) +
    geom_point(shape = 21, alpha = 0.8, aes(fill=GenTreatment,group=CellID), position = position_dodge(0.4), size = 1) +
    stat_summary(
      geom = "errorbar", 
      fun.min = mean, 
      fun = mean, 
      fun.max = mean, 
      width = .7,
      size = .4
    ) +
    stat_summary(
      geom = "linerange",
      fun.data = mean_se,
      size = 0.4
    ) +
    scale_fill_manual(values = c("white", "black")) +
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "firing frequency (Hz)",
      fill = "treatment"
    )
}

AG_KNDyPNA_gridSenktidePlot <- function(
  basePlot
){
  gridPlot <- basePlot %>% 
    facet(
      c("GenTreatment", "AgeGroup"),
      panel.labs.font = list(face = "bold", size = 11)
    )+
    rremove(
      "legend"
    ) +
    theme(
      strip.text = element_text(
        size = 11, 
        family = "Arial",
        face = "bold"),
      axis.text = element_text(
        size = 11,
        family = "Arial"
      ),
      axis.title = element_text(
        size = 11, 
        family = "Arial",
        face = "bold")
    )
  return(gridPlot)
}

AG_KNDyPNA_longSenktidePlot <- function(
  basePlot
){
  longPlot <- basePlot +
    facet_wrap(
      ~ TreatxAge,
      # ~ AgeGroup + GenTreatment,
      # strip.position = "bottom",
      ncol = 4,
      nrow = 1
    )+
    rremove(
      "legend"
    ) + 
    theme(
      # strip.background = element_blank(),
      # strip.placement = "outside",
      strip.text = element_text(size = 11, family = "Arial", face = "bold"),
      axis.text = element_text(size = 11, family = "Arial"),
      axis.title = element_text(size = 11, family = "Arial", face = "bold")
    )
  return(longPlot)
}

AG_KNDyPNA_plotCyclesPercent <- function(
  df,
  ylabel = "% days in stage",
  alpha = 0.8,
  dotSize = 1,
  barWidth = 0.7,
  barSize = 0.4,
  addMedian = FALSE,
  medianColor = "red",
  medianAlpha = 0.7,
  strip.position = "top"
){
  viz <- ggerrorplot(
    df,
    x = "GenTreatment",
    y = "percent",
    palette = c("white", "black"),
    add = "jitter", 
    facet.by = "stage",
    add.params = AG_KNDyPNA_jitterParams(size = dotSize, alpha = alpha),
    xlab = "",
    ylab = ylabel,
    error.plot = "linerange",
    strip.position = strip.position
  ) +
    expand_limits(y = 0) +
    rremove("xlab") +
    rremove("legend") + 
    addMeanHorizontalBar(width = barWidth, size = barSize)
  
  if(strip.position == "bottom"){
    viz <- viz + theme(
      strip.placement = "outside",
      strip.background = element_blank()
    )
  }
  
  viz <- viz + AG_KNDyPNA_textTheme()
    
  
  if(addMedian){
    viz <- viz + addMedianHorizontalBar(
      width = barWidth, 
      size = barSize,
      color = medianColor,
      alpha = medianAlpha
    )
  }
  
  return(viz)
}

AG_KNDyPNA_jitterParams <- function(
  size = 1,
  alpha = 0.8
){
  params <- list(
    shape = 21,
    fill = "GenTreatment",
    size = size,
    alpha = alpha
  )
  return(params)
}

AG_KNDyPNA_textTheme <- function(size = 11){
  theme = theme(
    text = element_text(size = size, family = "Arial"),
    strip.text = element_text(face = "bold", size = size),
    axis.title = element_text(face = "bold")
  )
  return(theme)  
}

addMeanHorizontalBar <- function(
  width = 0.7,
  size = 0.4
){
  stat_summary(
    geom = "errorbar", 
    fun.min = mean, 
    fun = mean, 
    fun.max = mean, 
    width = width,
    size = size
  )
}

addMedianHorizontalBar <- function(
  width = 0.7,
  size = 0.4,
  color = "red",
  alpha = 0.7
){
  stat_summary(
    geom = "errorbar", 
    fun.min = median, 
    fun = median, 
    fun.max = median, 
    width = width,
    size = size,
    color = color,
    alpha = alpha
  )
}

AG_KNDyPNA_plotCycleTraces <- function(
  df,
  day = day,
  stage = stage,
  mouseID = MouseID,
  ncol = NULL,
  nrow = NULL,
  lineColorVar = NULL,
  colorKey = c("CON" = "grey", "PNA" = "black"),
  removeFacets = FALSE,
  removeLegend = TRUE
){
  viz <- ggplot(df, aes(x = {{ day }}, y = {{ stage }}, color = {{ lineColorVar }})) +
    geom_line() +
    facet_wrap(
      vars( {{ mouseID }} ),
      ncol = ncol,
      nrow = nrow
    ) +
    scale_y_continuous(
      breaks = c(1, 2, 3), #axis ticks only at 1, 2, 3
      labels = c("E", "D", "P") #replace with E, D, and P
    ) +
    scale_x_continuous(
      breaks = seq(1, 21, 3) #labels every third integer
    ) +
    expand_limits(
      y = 0
    )
  
  viz <- viz + 
    theme_pubr() +
    AG_KNDyPNA_textTheme()
  
  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(values = colorKey)
  }
  
  if(removeLegend == TRUE) {
    viz <- viz + rremove("legend")
  } 
    
  if(removeFacets == TRUE) {
    viz <- viz + theme(
      strip.text = element_blank(), 
      strip.background = element_blank()
    )
  }
  
  
  return(viz)
}
