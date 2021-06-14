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

formatSenktideForIgor <- function(
  df, 
  timeRelBath, #How long after senktide hits bath should the analysis period start?
  analysisDur,
  saveToClipboard = FALSE
){
  formatedDF <- df %>%
    mutate(
      sctName = paste0(CellID, "_sct"),
      baselineStart = Senktide_bath - 1 - analysisDur,
      baselineEnd = Senktide_bath - 1,
      senktideStart = Senktide_bath + timeRelBath,
      senktideEnd = Senktide_bath + timeRelBath + analysisDur
    ) %>%
    select(
      sctName,
      CellID,
      TreatxAge,
      baselineStart,
      baselineEnd,
      senktideStart,
      senktideEnd,
      Senktide_bath
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

AG_KNDyPNA_makeLowerCase <- function(df){
  df <- df %>%
    mutate(
      AgeGroup = case_when(
        AgeGroup == "Adult" ~ "adult",
        AgeGroup == "Juvenile" ~ "juvenile"
        # AgeGroup == "Juvenile" ~ "3wk"
      ),
      GenTreatment = case_when(
        GenTreatment == "Control" ~ "control",
        TRUE ~ as.character(GenTreatment)
      )
    )
  
  df$AgeGroup <- factor(
    df$AgeGroup,
    levels = c("juvenile", "adult")
    # levels = c("3wk", "adult")
  )
  
  df$GenTreatment <- factor(
    df$GenTreatment,
    levels = c("control", "PNA")
  )
  
  return(df)
}