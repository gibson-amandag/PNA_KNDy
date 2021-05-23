generatePPT_byBurstParam <- function(
  burstParameters,
  niceNamesDF,
  binWidths,
  dotSizes,
  analysisKeyDF,
  analysisReason,
  demoDF,
  groupingVars,
  rateForQuiet,
  pptNameForSave
){
  burstPPT <- read_pptx()
  burstPPT <- addTitleSlide_ppt(burstPPT, "Burst Parameters")
  
  for(param in burstParameters){
    paramName <- niceNamesDF[,as.character(param)]
    binWidth <- as.numeric(binWidths[as.character(param)])
    if(is.na(binWidth)){
      binWidth = NULL
    }
    dotSize <- as.numeric(dotSizes[as.character(param)])
    
    
    for(row in 1:nrow(analysisKeyDF)){
      analysis <- analysisKeyDF[row, "specAnalysisName"]
      bw <- analysisKeyDF[row, "bw"]
      analysisName <- analysisKeyDF[row, "analysisName"]
      
      bParamsDF <- loadBurstParamsData(analysis, BurstOutputsFolder, demoDF)
      bParamsDF_contrasts <- makeTreatandAgeContrasts(bParamsDF)
      
      burstPPT <- addAnalysisTypeTitle_ppt(analysisName, bw, analysisReason, burstPPT)
      
      if(as.character(param) == "tf"){ #only add once
        burstPPT <- addCountLittersCellsFiringToPPT(bParamsDF, groupingVars, rateForQuiet, burstPPT)
      }
      
      addParamSlidesToPPT(
        bParamsDF, 
        bParamsDF_contrasts, 
        param, 
        paramName, 
        binWidth, 
        dotSize, 
        groupingVars, 
        burstPPT
      )
    }
  }
  pptTitle = paste0(pptNameForSave, ".pptx")
  print(burstPPT, target = file.path(DataOutputFolder, pptTitle))
}

generatePPT_byAnalysisType <- function(
  burstParameters,
  niceNamesDF,
  binWidths,
  dotSizes,
  analysisKeyDF,
  analysisReason,
  demoDF,
  groupingVars,
  rateForQuiet,
  pptNameForSave
){
  burstPPT <- read_pptx()
  burstPPT <- addTitleSlide_ppt(burstPPT, "Burst Parameters")
  
  for(row in 1:nrow(analysisKeyDF)){
    analysis <- analysisKeyDF[row, "specAnalysisName"]
    bw <- analysisKeyDF[row, "bw"]
    analysisName <- analysisKeyDF[row, "analysisName"]
    
    bParamsDF <- loadBurstParamsData(analysis, BurstOutputsFolder, demoDF)
    bParamsDF_contrasts <- makeTreatandAgeContrasts(bParamsDF)
  
    burstPPT <- addAnalysisTypeTitle_ppt(analysisName, bw, analysisReason, burstPPT)
    
    burstPPT <- addCountLittersCellsFiringToPPT(bParamsDF, groupingVars, rateForQuiet, burstPPT)
    
    for(param in burstParameters){
      paramName <- niceNamesDF[,as.character(param)]
      binWidth <- as.numeric(binWidths[as.character(param)])
      if(is.na(binWidth)){
        binWidth = NULL
      }
      dotSize <- as.numeric(dotSizes[as.character(param)])
    
      
      addParamSlidesToPPT(
        bParamsDF, 
        bParamsDF_contrasts, 
        param, 
        paramName, 
        binWidth, 
        dotSize, 
        groupingVars, 
        burstPPT
      )
    }
  }
  pptTitle = paste0(pptNameForSave, ".pptx")
  print(burstPPT, target = file.path(DataOutputFolder, pptTitle))
}