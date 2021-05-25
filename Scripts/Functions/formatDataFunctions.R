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
      EndTime_spont
    )
  return(formatedDF)
}