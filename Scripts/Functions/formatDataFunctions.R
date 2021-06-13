#Format dates function
format_dates = function(column){
  as.Date(column, format = "%Y-%m-%d")
}

formatForIgor <- function(df) {
  formatedDF <- df %>%
    mutate(
      sctName = paste0(CellID, "_sct")
    ) %>%
    select(
      sctName,
      CellID,
      TreatxAge,
      StartTime_spont,
      EndTime_spont,
      StartTime_senktide,
      Status,
    )
  return(formatedDF)
}

# param should be in form exprs(parameter), can be params[1] form from params being an exprs list
combineBurstBinsForParam <- function(
  df_0_20,
  df_20_40,
  df_40_60,
  param,
  demoDF = KNDyDATA
){
  selDF_0_20 <- df_0_20 %>%
    select(
      CellID,
      !!! param
    ) %>%
    rename(
      bin0_20 = as.character(param)
    )
  
  selDF_20_40 <- df_20_40 %>%
    select(
      CellID,
      !!! param
    ) %>%
    rename(
      bin20_40 = as.character(param)
    )
  
  selDF_40_60 <- df_40_60 %>%
    select(
      CellID,
      !!! param
    ) %>%
    rename(
      bin40_60 = as.character(param)
    )
  
  mergedDF <- selDF_0_20 %>%
    left_join(
      selDF_20_40,
      by = "CellID"
    ) %>%
    left_join(
      selDF_40_60,
      by = "CellID"
    )
  
  mergedDF <- getBurstParamsDF(KNDyDATA, mergedDF)
  
  return(mergedDF$bParamsDF)
}