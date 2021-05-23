### Load DataFrame from Excel --------------------------------------------------
myXLSX_func = function(folderPath, fileName, sheetName){
  read.xlsx(
    file.path(folderPath, fileName),
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}

myXLSX_funcFileSelect = function(selPath, sheetName){
  read.xlsx(
    selPath,
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}

loadBurstParamsData <- function(analysis, BurstOutputsFolder, demoDF){
  fileName = paste0(analysis, ".txt")
  bParamsInit <- read.csv(file.path(BurstOutputsFolder, fileName), sep = "\t")
  
  # Add the demographic info to the burst params dataframe
  bParamsOut <- getBurstParamsDF(demoDF, bParamsInit)
  bParamsDF <- bParamsOut$bParamsDF
  bParamsDF <- filterData(bParamsDF)
  return(bParamsDF)
  
}