##Notes
#Save the object name to a string 
#deparse(substitute(obj)
#as.character may also be useful

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

#### Get Data Ready-------------------------------------------------------------

#Format dates function
format_dates = function(column){
  as.Date(column, format = "%Y-%m-%d")
}

#### Separate by Age Function
sep_by_age = function(df){
  df_nam <- ensym(df) #capture name of dataframe
  df_juv  <- df%>% #only cells from juvenile mice
    filter(AgeGroup == "Juvenile")
  
  df_adult <- df%>% #only cells from adult mice
    filter(AgeGroup == "Adult")
  
  return(
    list(
      "df_juv" = df_juv,
      "df_adult" = df_adult
    )
  )
  
  # nam_juv = paste0(df_nam, "_juv") #name for juvenile dataframe
  # nam_adult = paste0(df_nam, "_adult") #name for adult dataframe
  # 
  # assign(nam_juv, df_juv, envir = .GlobalEnv) #assign juvenile df to juv df name in global environment
  # assign(nam_adult, df_adult, envir = .GlobalEnv) #assign adult df to adult df name in global environment
}

####Exclude function
excludeFunc = function(df){
  df = df %>%
    filter(Exclude == FALSE | is.na(Exclude)) #remove excluded cells
}

#### Summarize Variables 
#Provide string name for columns for both var_toSummarize and group_vars. Can group on multiple variables
KNDy_summary_byGroup = function(var_toSummarize, df, group_vars, addName = TRUE){
  var_name = KNDy_VarNames[, var_toSummarize] #get the long name
  df = df %>%
    filter(! is.na(!!!syms(var_toSummarize))) %>%
    group_by(!!!syms(group_vars)) %>% #this evaluates the string name to the symbols here
    summarize(
      Mean = mean(!!!syms(var_toSummarize), na.rm = TRUE),
      SD = sd(!!!syms(var_toSummarize), na.rm = TRUE),
      n = n(),
      SEM = SD/sqrt(n),
      .groups = 'drop'
    )
  if(addName){
    df <- df %>%
      mutate(
        Variable = var_toSummarize, #add columns to indicate what is summarized in each row
        VarName = var_name
      )
  }
  return(df)
}
#Provide string name for columns for both var_toSummarize and group_vars. Can group on multiple variables
KNDy_quantileSummary <- function( varToSummarize, df, group_vars, addName = TRUE) {
  var_name = KNDy_VarNames[, varToSummarize]
  df <- df %>%
    filter(! is.na(!!!syms(varToSummarize))) %>%
    group_by(!!!syms(group_vars)) %>%
    summarize(
      min = min(!!!syms(varToSummarize), na.rm = TRUE),
      q1 = quantile(!!!syms(varToSummarize), 0.25, na.rm = TRUE),
      median = median(!!!syms(varToSummarize), na.rm=TRUE),
      q3 = quantile(!!!syms(varToSummarize), 0.75, na.rm=TRUE),
      max = max(!!!syms(varToSummarize), na.rm = TRUE),
      .groups = 'drop'
    )
  
  if(addName){
    df <- df %>%
      mutate(
        Variable = varToSummarize,
        VarName = var_name
      )
  }
  
  return(df)
}

# #test 1 - works on its own for one variable to summarize
# KNDy_summary_byGroup("Mbd_spont", KNDy_burst_spont, group_vars = c("GenTreatment", "AgeGroup"))
# 
# #test 2 - provide a list of variables to summarize. Use map_dfr which will combine all of the summaries into a single data frame by binding the rows. Use a single set of grouping variables
# map_dfr(BurstVars_spont_quo, KNDy_summary_byGroup, KNDy_burst_spont, c("GenTreatment", "AgeGroup"))

KNDy_summarize = function(
  vars_toSummarize_list,
  df_toSummarize,
  byAge = TRUE,
  byQuiet = FALSE,
  whichTreatment = "GenTreatment",
  df_name = FALSE #provide a specific alternative name for resulting data frame
) {
  if(df_name == FALSE){
    df_name = deparse(substitute(df_toSummarize)) #name of dataframe
  }
  
  
  if (byAge == TRUE) { #group by age (AgeGroup)
    df_TreatJuv = map_dfr( #combine into one dataframe with row binding
      vars_toSummarize_list, #map across variable list (var_toSummarize)
      KNDy_summary_byGroup, #use the KNDy_summary_byGroup formula
      df_toSummarize, #df input
      c(whichTreatment, "AgeGroup") #group_vars input
    )
    assign(
      paste0(df_name, "_sumTreatJuv"), #name
      df_TreatJuv, #dataframe
      envir = .GlobalEnv #environment (save outside function)
    )
    
    df_TreatJuvSac = map_dfr(
      vars_toSummarize_list,
      KNDy_summary_byGroup,
      df_toSummarize,
      c(whichTreatment, "AgeGroup", "Sac_9plus")
    )
    assign(
      paste0(df_name, "_sumTreatJuvSac"),
      df_TreatJuvSac, 
      envir = .GlobalEnv
    )
    
    if (byQuiet == TRUE) {
      #group by quiescence
      df_TreatJuvQuiet = map_dfr(
        vars_toSummarize_list,
        KNDy_summary_byGroup,
        df_toSummarize,
        c(whichTreatment, "AgeGroup", "Quiet")
      )
      assign(
        paste0(df_name, "_sumTreatJuvQuiet"),
        df_TreatJuvQuiet,
        envir = .GlobalEnv
      )
      
      df_TreatJuvSacQuiet = map_dfr(
        vars_toSummarize_list,
        KNDy_summary_byGroup,
        df_toSummarize,
        c(whichTreatment, "AgeGroup", "Sac_9plus", "Quiet")
      )
      assign(
        paste0(df_name, "_sumTreatJuvSacQuiet"),
        df_TreatJuvSacQuiet,
        envir = .GlobalEnv
      )
    }
  }
  
  if (byAge == FALSE) {
    #do not group by age (AgeGroup)
    df_Treat = map_dfr(
      vars_toSummarize_list,
      KNDy_summary_byGroup,
      df_toSummarize,
      whichTreatment
    )
    assign(
      paste0(df_name, "_sumTreat"), 
      df_Treat, 
      envir = .GlobalEnv
    )
    
    df_TreatSac = map_dfr(
      vars_toSummarize_list,
      KNDy_summary_byGroup,
      df_toSummarize,
      c(whichTreatment, "Sac_9plus")
    )
    assign(
      paste0(df_name, "_sumTreatSac"), 
      df_TreatSac, 
      envir = .GlobalEnv
    )
    
    if (byQuiet == TRUE) {
      #group by quiescence
      df_TreatQuiet = map_dfr(
        vars_toSummarize_list,
        KNDy_summary_byGroup,
        df_toSummarize,
        c(whichTreatment, "Quiet")
      )
      assign(
        paste0(df_name, "_sumTreatQuiet"),
        df_TreatQuiet,
        envir = .GlobalEnv
      )
      
      df_TreatSacQuiet = map_dfr(
        vars_toSummarize_list,
        KNDy_summary_byGroup,
        df_toSummarize,
        c(whichTreatment, "Sac_9plus", "Quiet")
      )
      assign(
        paste0(df_name, "_sumTreatSacQuiet"),
        df_TreatSacQuiet,
        envir = .GlobalEnv
      )
    }
  }
}



####Write to Excel Files 
#uses openxslx

writeToWorkbook = function(sheetName, df, wb){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = "TableStyleMedium2")
}

### Group by Treatment and Age ------------------------------------------------------------------------
groupByTreatAge <- function(df){
  df_byTreatAge <- df %>%
    group_by(GenTreatment, AgeGroup)
  return(df_byTreatAge)
}

#Summarize by TreatAge
AvgByTreatAge_func <- function(df){
  AvgByTreatAge <- df %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  
  return(AvgByTreatAge)
}

#Evaluate both in one function
getAvgByTreatAge <- function(df){
  grouped_df <- groupByTreatAge(df)
  avg_df <- AvgByTreatAge_func(grouped_df)
  return(avg_df)
}

###Make Long Form

make_long_form_burstsPerWindow = function(df){
  df %>%
    gather(key = "BurstWindow", value = "BurstsPerHour", c(BurstsPerHour_0.01:BurstsPerHour_1.00), factor_key=TRUE)
}

#Add a Burst Window column in miliseconds
make_BW_col = function(df){
  df = df %>%
    mutate(BW_msec = case_when(
      BurstWindow == "BurstsPerHour_0.01" ~ 10,
      BurstWindow == "BurstsPerHour_0.02" ~ 20,
      BurstWindow == "BurstsPerHour_0.03" ~ 30,
      BurstWindow == "BurstsPerHour_0.04" ~ 40,
      BurstWindow == "BurstsPerHour_0.05" ~ 50,
      BurstWindow == "BurstsPerHour_0.06" ~ 60,
      BurstWindow == "BurstsPerHour_0.07" ~ 70,
      BurstWindow == "BurstsPerHour_0.08" ~ 80,
      BurstWindow == "BurstsPerHour_0.09" ~ 90,
      BurstWindow == "BurstsPerHour_0.10" ~ 100,
      BurstWindow == "BurstsPerHour_0.11" ~ 110,
      BurstWindow == "BurstsPerHour_0.12" ~ 120,
      BurstWindow == "BurstsPerHour_0.13" ~ 130,
      BurstWindow == "BurstsPerHour_0.14" ~ 140,
      BurstWindow == "BurstsPerHour_0.15" ~ 150,
      BurstWindow == "BurstsPerHour_0.16" ~ 160,
      BurstWindow == "BurstsPerHour_0.17" ~ 170,
      BurstWindow == "BurstsPerHour_0.18" ~ 180,
      BurstWindow == "BurstsPerHour_0.19" ~ 190,
      BurstWindow == "BurstsPerHour_0.20" ~ 200,
      BurstWindow == "BurstsPerHour_0.21" ~ 210,
      BurstWindow == "BurstsPerHour_0.22" ~ 220,
      BurstWindow == "BurstsPerHour_0.23" ~ 230,
      BurstWindow == "BurstsPerHour_0.24" ~ 240,
      BurstWindow == "BurstsPerHour_0.25" ~ 250,
      BurstWindow == "BurstsPerHour_0.26" ~ 260,
      BurstWindow == "BurstsPerHour_0.27" ~ 270,
      BurstWindow == "BurstsPerHour_0.28" ~ 280,
      BurstWindow == "BurstsPerHour_0.29" ~ 290,
      BurstWindow == "BurstsPerHour_0.30" ~ 300,
      BurstWindow == "BurstsPerHour_0.31" ~ 310,
      BurstWindow == "BurstsPerHour_0.32" ~ 320,
      BurstWindow == "BurstsPerHour_0.33" ~ 330,
      BurstWindow == "BurstsPerHour_0.34" ~ 340,
      BurstWindow == "BurstsPerHour_0.35" ~ 350,
      BurstWindow == "BurstsPerHour_0.36" ~ 360,
      BurstWindow == "BurstsPerHour_0.37" ~ 370,
      BurstWindow == "BurstsPerHour_0.38" ~ 380,
      BurstWindow == "BurstsPerHour_0.39" ~ 390,
      BurstWindow == "BurstsPerHour_0.40" ~ 400,
      BurstWindow == "BurstsPerHour_0.41" ~ 410,
      BurstWindow == "BurstsPerHour_0.42" ~ 420,
      BurstWindow == "BurstsPerHour_0.43" ~ 430,
      BurstWindow == "BurstsPerHour_0.44" ~ 440,
      BurstWindow == "BurstsPerHour_0.45" ~ 450,
      BurstWindow == "BurstsPerHour_0.46" ~ 460,
      BurstWindow == "BurstsPerHour_0.47" ~ 470,
      BurstWindow == "BurstsPerHour_0.48" ~ 480,
      BurstWindow == "BurstsPerHour_0.49" ~ 490,
      BurstWindow == "BurstsPerHour_0.50" ~ 500,
      BurstWindow == "BurstsPerHour_0.51" ~ 510,
      BurstWindow == "BurstsPerHour_0.52" ~ 520,
      BurstWindow == "BurstsPerHour_0.53" ~ 530,
      BurstWindow == "BurstsPerHour_0.54" ~ 540,
      BurstWindow == "BurstsPerHour_0.55" ~ 550,
      BurstWindow == "BurstsPerHour_0.56" ~ 560,
      BurstWindow == "BurstsPerHour_0.57" ~ 570,
      BurstWindow == "BurstsPerHour_0.58" ~ 580,
      BurstWindow == "BurstsPerHour_0.59" ~ 590,
      BurstWindow == "BurstsPerHour_0.60" ~ 600,
      BurstWindow == "BurstsPerHour_0.61" ~ 610,
      BurstWindow == "BurstsPerHour_0.62" ~ 620,
      BurstWindow == "BurstsPerHour_0.63" ~ 630,
      BurstWindow == "BurstsPerHour_0.64" ~ 640,
      BurstWindow == "BurstsPerHour_0.65" ~ 650,
      BurstWindow == "BurstsPerHour_0.66" ~ 660,
      BurstWindow == "BurstsPerHour_0.67" ~ 670,
      BurstWindow == "BurstsPerHour_0.68" ~ 680,
      BurstWindow == "BurstsPerHour_0.69" ~ 690,
      BurstWindow == "BurstsPerHour_0.70" ~ 700,
      BurstWindow == "BurstsPerHour_0.71" ~ 710,
      BurstWindow == "BurstsPerHour_0.72" ~ 720,
      BurstWindow == "BurstsPerHour_0.73" ~ 730,
      BurstWindow == "BurstsPerHour_0.74" ~ 740,
      BurstWindow == "BurstsPerHour_0.75" ~ 750,
      BurstWindow == "BurstsPerHour_0.76" ~ 760,
      BurstWindow == "BurstsPerHour_0.77" ~ 770,
      BurstWindow == "BurstsPerHour_0.78" ~ 780,
      BurstWindow == "BurstsPerHour_0.79" ~ 790,
      BurstWindow == "BurstsPerHour_0.80" ~ 800,
      BurstWindow == "BurstsPerHour_0.81" ~ 810,
      BurstWindow == "BurstsPerHour_0.82" ~ 820,
      BurstWindow == "BurstsPerHour_0.83" ~ 830,
      BurstWindow == "BurstsPerHour_0.84" ~ 840,
      BurstWindow == "BurstsPerHour_0.85" ~ 850,
      BurstWindow == "BurstsPerHour_0.86" ~ 860,
      BurstWindow == "BurstsPerHour_0.87" ~ 870,
      BurstWindow == "BurstsPerHour_0.88" ~ 880,
      BurstWindow == "BurstsPerHour_0.89" ~ 890,
      BurstWindow == "BurstsPerHour_0.90" ~ 900,
      BurstWindow == "BurstsPerHour_0.91" ~ 910,
      BurstWindow == "BurstsPerHour_0.92" ~ 920,
      BurstWindow == "BurstsPerHour_0.93" ~ 930,
      BurstWindow == "BurstsPerHour_0.94" ~ 940,
      BurstWindow == "BurstsPerHour_0.95" ~ 950,
      BurstWindow == "BurstsPerHour_0.96" ~ 960,
      BurstWindow == "BurstsPerHour_0.97" ~ 970,
      BurstWindow == "BurstsPerHour_0.98" ~ 980,
      BurstWindow == "BurstsPerHour_0.99" ~ 990,
      BurstWindow == "BurstsPerHour_1.00" ~ 1000)
    )
}

make_long_form_burstsPerWindow_hour1 = function(df){
  df %>%
    gather(key = "BurstWindow", value = "BurstsPerHour", c(BurstsHour1_0.01:BurstsHour1_1.00), factor_key=TRUE)
}

#Add a Burst Window column in miliseconds
make_BW_col_hour1 = function(df){
  df = df %>%
    mutate(BW_msec = case_when(
      BurstWindow == "BurstsHour1_0.01" ~ 10,
      BurstWindow == "BurstsHour1_0.02" ~ 20,
      BurstWindow == "BurstsHour1_0.03" ~ 30,
      BurstWindow == "BurstsHour1_0.04" ~ 40,
      BurstWindow == "BurstsHour1_0.05" ~ 50,
      BurstWindow == "BurstsHour1_0.06" ~ 60,
      BurstWindow == "BurstsHour1_0.07" ~ 70,
      BurstWindow == "BurstsHour1_0.08" ~ 80,
      BurstWindow == "BurstsHour1_0.09" ~ 90,
      BurstWindow == "BurstsHour1_0.10" ~ 100,
      BurstWindow == "BurstsHour1_0.11" ~ 110,
      BurstWindow == "BurstsHour1_0.12" ~ 120,
      BurstWindow == "BurstsHour1_0.13" ~ 130,
      BurstWindow == "BurstsHour1_0.14" ~ 140,
      BurstWindow == "BurstsHour1_0.15" ~ 150,
      BurstWindow == "BurstsHour1_0.16" ~ 160,
      BurstWindow == "BurstsHour1_0.17" ~ 170,
      BurstWindow == "BurstsHour1_0.18" ~ 180,
      BurstWindow == "BurstsHour1_0.19" ~ 190,
      BurstWindow == "BurstsHour1_0.20" ~ 200,
      BurstWindow == "BurstsHour1_0.21" ~ 210,
      BurstWindow == "BurstsHour1_0.22" ~ 220,
      BurstWindow == "BurstsHour1_0.23" ~ 230,
      BurstWindow == "BurstsHour1_0.24" ~ 240,
      BurstWindow == "BurstsHour1_0.25" ~ 250,
      BurstWindow == "BurstsHour1_0.26" ~ 260,
      BurstWindow == "BurstsHour1_0.27" ~ 270,
      BurstWindow == "BurstsHour1_0.28" ~ 280,
      BurstWindow == "BurstsHour1_0.29" ~ 290,
      BurstWindow == "BurstsHour1_0.30" ~ 300,
      BurstWindow == "BurstsHour1_0.31" ~ 310,
      BurstWindow == "BurstsHour1_0.32" ~ 320,
      BurstWindow == "BurstsHour1_0.33" ~ 330,
      BurstWindow == "BurstsHour1_0.34" ~ 340,
      BurstWindow == "BurstsHour1_0.35" ~ 350,
      BurstWindow == "BurstsHour1_0.36" ~ 360,
      BurstWindow == "BurstsHour1_0.37" ~ 370,
      BurstWindow == "BurstsHour1_0.38" ~ 380,
      BurstWindow == "BurstsHour1_0.39" ~ 390,
      BurstWindow == "BurstsHour1_0.40" ~ 400,
      BurstWindow == "BurstsHour1_0.41" ~ 410,
      BurstWindow == "BurstsHour1_0.42" ~ 420,
      BurstWindow == "BurstsHour1_0.43" ~ 430,
      BurstWindow == "BurstsHour1_0.44" ~ 440,
      BurstWindow == "BurstsHour1_0.45" ~ 450,
      BurstWindow == "BurstsHour1_0.46" ~ 460,
      BurstWindow == "BurstsHour1_0.47" ~ 470,
      BurstWindow == "BurstsHour1_0.48" ~ 480,
      BurstWindow == "BurstsHour1_0.49" ~ 490,
      BurstWindow == "BurstsHour1_0.50" ~ 500,
      BurstWindow == "BurstsHour1_0.51" ~ 510,
      BurstWindow == "BurstsHour1_0.52" ~ 520,
      BurstWindow == "BurstsHour1_0.53" ~ 530,
      BurstWindow == "BurstsHour1_0.54" ~ 540,
      BurstWindow == "BurstsHour1_0.55" ~ 550,
      BurstWindow == "BurstsHour1_0.56" ~ 560,
      BurstWindow == "BurstsHour1_0.57" ~ 570,
      BurstWindow == "BurstsHour1_0.58" ~ 580,
      BurstWindow == "BurstsHour1_0.59" ~ 590,
      BurstWindow == "BurstsHour1_0.60" ~ 600,
      BurstWindow == "BurstsHour1_0.61" ~ 610,
      BurstWindow == "BurstsHour1_0.62" ~ 620,
      BurstWindow == "BurstsHour1_0.63" ~ 630,
      BurstWindow == "BurstsHour1_0.64" ~ 640,
      BurstWindow == "BurstsHour1_0.65" ~ 650,
      BurstWindow == "BurstsHour1_0.66" ~ 660,
      BurstWindow == "BurstsHour1_0.67" ~ 670,
      BurstWindow == "BurstsHour1_0.68" ~ 680,
      BurstWindow == "BurstsHour1_0.69" ~ 690,
      BurstWindow == "BurstsHour1_0.70" ~ 700,
      BurstWindow == "BurstsHour1_0.71" ~ 710,
      BurstWindow == "BurstsHour1_0.72" ~ 720,
      BurstWindow == "BurstsHour1_0.73" ~ 730,
      BurstWindow == "BurstsHour1_0.74" ~ 740,
      BurstWindow == "BurstsHour1_0.75" ~ 750,
      BurstWindow == "BurstsHour1_0.76" ~ 760,
      BurstWindow == "BurstsHour1_0.77" ~ 770,
      BurstWindow == "BurstsHour1_0.78" ~ 780,
      BurstWindow == "BurstsHour1_0.79" ~ 790,
      BurstWindow == "BurstsHour1_0.80" ~ 800,
      BurstWindow == "BurstsHour1_0.81" ~ 810,
      BurstWindow == "BurstsHour1_0.82" ~ 820,
      BurstWindow == "BurstsHour1_0.83" ~ 830,
      BurstWindow == "BurstsHour1_0.84" ~ 840,
      BurstWindow == "BurstsHour1_0.85" ~ 850,
      BurstWindow == "BurstsHour1_0.86" ~ 860,
      BurstWindow == "BurstsHour1_0.87" ~ 870,
      BurstWindow == "BurstsHour1_0.88" ~ 880,
      BurstWindow == "BurstsHour1_0.89" ~ 890,
      BurstWindow == "BurstsHour1_0.90" ~ 900,
      BurstWindow == "BurstsHour1_0.91" ~ 910,
      BurstWindow == "BurstsHour1_0.92" ~ 920,
      BurstWindow == "BurstsHour1_0.93" ~ 930,
      BurstWindow == "BurstsHour1_0.94" ~ 940,
      BurstWindow == "BurstsHour1_0.95" ~ 950,
      BurstWindow == "BurstsHour1_0.96" ~ 960,
      BurstWindow == "BurstsHour1_0.97" ~ 970,
      BurstWindow == "BurstsHour1_0.98" ~ 980,
      BurstWindow == "BurstsHour1_0.99" ~ 990,
      BurstWindow == "BurstsHour1_1.00" ~ 1000)
    )
}

##### ANOVAs #########################
#Lots of help and inspiration from http://www.understandingdata.net/2017/05/11/anova-tables-in-r/
makeTreatandAgeContrasts <- function(df){
  #Make a contrasts data frame for use with anovas
  df_contrasts <- excludeFunc(df)
  #Make GenTreatment a factor
  df_contrasts$GenTreatment = as.factor(df_contrasts$GenTreatment)
  #Use contr.Sum for contrast
  contrasts(df_contrasts$GenTreatment) <- contr.Sum
  #Make AgeGroup a factor
  df_contrasts$AgeGroup <- as.factor(df_contrasts$AgeGroup)
  #use contr.Sum for contrast
  contrasts(df_contrasts$AgeGroup) <- contr.Sum
  return(df_contrasts)
}

lm_byTreatxAge <- function(
  response_var,
  df
){
  df <- df %>%
    filter(!is.na(!! response_var))
  model <- lm(eval(response_var) ~ GenTreatment * AgeGroup, data = df)
}

makeANOVA_TreatAge <- function(
  lm
){
  ANOVA <- Anova(lm, type = "III")
  #remove intercept and reorder - Interaction, Treatment, Age Group, Residuals
  ANOVA <- ANOVA[c(4, 2, 3, 5),]
  #Update column names and row names
  colnames(ANOVA) <- c("SS", "df", "<i>F</i>", "<i>p</i>")
  rownames(ANOVA) <- c("Interaction", "Treatment", "Age Group", "Residuals")
  #hide missing values in table
  options(knitr.kable.NA = '')
  kabled_ANOVA <- kable(ANOVA, digits = 3, escape = FALSE)
  return(kabled_ANOVA)
}

makeANOVAgetVals_TreatAge <- function(
  lm
){
  ANOVA <- Anova(lm, type = "III")
  #remove intercept and reorder - Interaction, Treatment, Age Group, Residuals
  ANOVA <- ANOVA[c(4, 2, 3, 5),]
  #Update column names and row names
  colnames(ANOVA) <- c("SS", "df", "<i>F</i>", "<i>p</i>")
  rownames(ANOVA) <- c("Interaction", "Treatment", "Age Group", "Residuals")
  #hide missing values in table
  options(knitr.kable.NA = '')
  kabled_ANOVA <- kable(ANOVA, digits = 3, escape = FALSE)
  
  p_interaction <- ANOVA[1, 4]
  p_treat <- ANOVA[2, 4]
  p_age <- ANOVA[3, 4]
  return(
    list(
      kabled_ANOVA = kabled_ANOVA,
      p_interaction = p_interaction,
      p_treat = p_treat,
      p_age = p_age
    )
  )
}

# slicer by Treatment - use with juvenile only df

makeTreatandWhoContrasts <- function(df){
  #Make a contrasts data frame for use with anovas
  df_contrasts <- excludeFunc(df)
  #Make GenTreatment a factor
  df_contrasts$GenTreatment = as.factor(df_contrasts$GenTreatment)
  #Use contr.Sum for contrast
  contrasts(df_contrasts$GenTreatment) <- contr.Sum
  #Make AgeGroup a factor
  df_contrasts$Who <- as.factor(df_contrasts$Who)
  #use contr.Sum for contrast
  contrasts(df_contrasts$AgeGroup) <- contr.Sum
  return(df_contrasts)
}

lm_byTreatxWho <- function(
  response_var,
  df
){
  df <- df %>%
    filter(!is.na(!! response_var))
  model <- lm(eval(response_var) ~ GenTreatment * Who, data = df)
}

makeANOVA_TreatWho <- function(
  lm
){
  ANOVA <- Anova(lm, type = "III")
  #remove intercept and reorder - Interaction, Treatment, Who, Residuals
  ANOVA <- ANOVA[c(4, 2, 3, 5),]
  #Update column names and row names
  colnames(ANOVA) <- c("SS", "df", "<i>F</i>", "<i>p</i>")
  rownames(ANOVA) <- c("Interaction", "Treatment", "Experimenter", "Residuals")
  #hide missing values in table
  options(knitr.kable.NA = '')
  kabled_ANOVA <- kable(ANOVA, digits = 3, escape = FALSE)
  return(kabled_ANOVA)
}

# Recorder by Treatment - use with juvenile only df

makeTreatandWhoRecordedContrasts <- function(df){
  #Make a contrasts data frame for use with anovas
  df_contrasts <- excludeFunc(df)
  #Make GenTreatment a factor
  df_contrasts$GenTreatment = as.factor(df_contrasts$GenTreatment)
  #Use contr.Sum for contrast
  contrasts(df_contrasts$GenTreatment) <- contr.Sum
  #Make WhoRecorded a factor
  df_contrasts$WhoRecorded <- as.factor(df_contrasts$WhoRecorded)
  #use contr.Sum for contrast
  contrasts(df_contrasts$AgeGroup) <- contr.Sum
  return(df_contrasts)
}

lm_byTreatxWhoRecorded <- function(
  response_var,
  df
){
  df <- df %>%
    filter(!is.na(!! response_var))
  model <- lm(eval(response_var) ~ GenTreatment * WhoRecorded, data = df)
}

makeANOVA_TreatWhoRecorded <- function(
  lm
){
  ANOVA <- Anova(lm, type = "III")
  #remove intercept and reorder - Interaction, Treatment, Who Recorded, Residuals
  ANOVA <- ANOVA[c(4, 2, 3, 5),]
  #Update column names and row names
  colnames(ANOVA) <- c("SS", "df", "<i>F</i>", "<i>p</i>")
  rownames(ANOVA) <- c("Interaction", "Treatment", "Experimenter", "Residuals")
  #hide missing values in table
  options(knitr.kable.NA = '')
  kabled_ANOVA <- kable(ANOVA, digits = 3, escape = FALSE)
  return(kabled_ANOVA)
}


###### PLOTS ##########################

## Cycle Plot Preparation-------
make_cycles_long <- function(df){
  df %>%
    #drop_na(Day1:Day21) %>%
    gather(
      key = "DayNum",
      value = "Stage",
      c(Day1:Day21),
      factor_key = TRUE
    ) %>%
    drop_na(Stage)
}

add_Day_col <- function(df){
  df <- df %>%
    mutate(
      Day = 
        case_when(
          DayNum == "Day1" ~ 1,
          DayNum == "Day2" ~ 2,
          DayNum == "Day3" ~ 3,
          DayNum == "Day4" ~ 4,
          DayNum == "Day5" ~ 5,
          DayNum == "Day6" ~ 6,
          DayNum == "Day7" ~ 7,
          DayNum == "Day8" ~ 8,
          DayNum == "Day9" ~ 9,
          DayNum == "Day10" ~ 10,
          DayNum == "Day11" ~ 11,
          DayNum == "Day12" ~ 12,
          DayNum == "Day13" ~ 13,
          DayNum == "Day14" ~ 14,
          DayNum == "Day15" ~ 15,
          DayNum == "Day16" ~ 16,
          DayNum == "Day17" ~ 17,
          DayNum == "Day18" ~ 18,
          DayNum == "Day19" ~ 19,
          DayNum == "Day20" ~ 20,
          DayNum == "Day21" ~ 21
        )
    )
  return(df)
}

#firing rate plot prep -----
#make long
make_firing_long <- function(df){
  df %>%
    gather(
      key = "Minute",
      value = "FiringRate_Hz",
      c(FreqHz_0:FreqHz_180),
      factor_key = TRUE
    ) %>%
    drop_na(FiringRate_Hz) #drop rows with NAs in FiringRate_Hz column
}

#Add minute number column
add_Min_col <- function(df){
  df <- df %>%
    mutate(
      Min_num = 
        case_when(
          Minute == "FreqHz_0" ~ 0,
          Minute == "FreqHz_1" ~ 1,
          Minute == "FreqHz_2" ~ 2,
          Minute == "FreqHz_3" ~ 3,
          Minute == "FreqHz_4" ~ 4,
          Minute == "FreqHz_5" ~ 5,
          Minute == "FreqHz_6" ~ 6,
          Minute == "FreqHz_7" ~ 7,
          Minute == "FreqHz_8" ~ 8,
          Minute == "FreqHz_9" ~ 9,
          Minute == "FreqHz_10" ~ 10,
          Minute == "FreqHz_11" ~ 11,
          Minute == "FreqHz_12" ~ 12,
          Minute == "FreqHz_13" ~ 13,
          Minute == "FreqHz_14" ~ 14,
          Minute == "FreqHz_15" ~ 15,
          Minute == "FreqHz_16" ~ 16,
          Minute == "FreqHz_17" ~ 17,
          Minute == "FreqHz_18" ~ 18,
          Minute == "FreqHz_19" ~ 19,
          Minute == "FreqHz_20" ~ 20,
          Minute == "FreqHz_21" ~ 21,
          Minute == "FreqHz_22" ~ 22,
          Minute == "FreqHz_23" ~ 23,
          Minute == "FreqHz_24" ~ 24,
          Minute == "FreqHz_25" ~ 25,
          Minute == "FreqHz_26" ~ 26,
          Minute == "FreqHz_27" ~ 27,
          Minute == "FreqHz_28" ~ 28,
          Minute == "FreqHz_29" ~ 29,
          Minute == "FreqHz_30" ~ 30,
          Minute == "FreqHz_31" ~ 31,
          Minute == "FreqHz_32" ~ 32,
          Minute == "FreqHz_33" ~ 33,
          Minute == "FreqHz_34" ~ 34,
          Minute == "FreqHz_35" ~ 35,
          Minute == "FreqHz_36" ~ 36,
          Minute == "FreqHz_37" ~ 37,
          Minute == "FreqHz_38" ~ 38,
          Minute == "FreqHz_39" ~ 39,
          Minute == "FreqHz_40" ~ 40,
          Minute == "FreqHz_41" ~ 41,
          Minute == "FreqHz_42" ~ 42,
          Minute == "FreqHz_43" ~ 43,
          Minute == "FreqHz_44" ~ 44,
          Minute == "FreqHz_45" ~ 45,
          Minute == "FreqHz_46" ~ 46,
          Minute == "FreqHz_47" ~ 47,
          Minute == "FreqHz_48" ~ 48,
          Minute == "FreqHz_49" ~ 49,
          Minute == "FreqHz_50" ~ 50,
          Minute == "FreqHz_51" ~ 51,
          Minute == "FreqHz_52" ~ 52,
          Minute == "FreqHz_53" ~ 53,
          Minute == "FreqHz_54" ~ 54,
          Minute == "FreqHz_55" ~ 55,
          Minute == "FreqHz_56" ~ 56,
          Minute == "FreqHz_57" ~ 57,
          Minute == "FreqHz_58" ~ 58,
          Minute == "FreqHz_59" ~ 59,
          Minute == "FreqHz_60" ~ 60,
          Minute == "FreqHz_61" ~ 61,
          Minute == "FreqHz_62" ~ 62,
          Minute == "FreqHz_63" ~ 63,
          Minute == "FreqHz_64" ~ 64,
          Minute == "FreqHz_65" ~ 65,
          Minute == "FreqHz_66" ~ 66,
          Minute == "FreqHz_67" ~ 67,
          Minute == "FreqHz_68" ~ 68,
          Minute == "FreqHz_69" ~ 69,
          Minute == "FreqHz_70" ~ 70,
          Minute == "FreqHz_71" ~ 71,
          Minute == "FreqHz_72" ~ 72,
          Minute == "FreqHz_73" ~ 73,
          Minute == "FreqHz_74" ~ 74,
          Minute == "FreqHz_75" ~ 75,
          Minute == "FreqHz_76" ~ 76,
          Minute == "FreqHz_77" ~ 77,
          Minute == "FreqHz_78" ~ 78,
          Minute == "FreqHz_79" ~ 79,
          Minute == "FreqHz_80" ~ 80,
          Minute == "FreqHz_81" ~ 81,
          Minute == "FreqHz_82" ~ 82,
          Minute == "FreqHz_83" ~ 83,
          Minute == "FreqHz_84" ~ 84,
          Minute == "FreqHz_85" ~ 85,
          Minute == "FreqHz_86" ~ 86,
          Minute == "FreqHz_87" ~ 87,
          Minute == "FreqHz_88" ~ 88,
          Minute == "FreqHz_89" ~ 89,
          Minute == "FreqHz_90" ~ 90,
          Minute == "FreqHz_91" ~ 91,
          Minute == "FreqHz_92" ~ 92,
          Minute == "FreqHz_93" ~ 93,
          Minute == "FreqHz_94" ~ 94,
          Minute == "FreqHz_95" ~ 95,
          Minute == "FreqHz_96" ~ 96,
          Minute == "FreqHz_97" ~ 97,
          Minute == "FreqHz_98" ~ 98,
          Minute == "FreqHz_99" ~ 99,
          Minute == "FreqHz_100" ~ 100,
          Minute == "FreqHz_101" ~ 101,
          Minute == "FreqHz_102" ~ 102,
          Minute == "FreqHz_103" ~ 103,
          Minute == "FreqHz_104" ~ 104,
          Minute == "FreqHz_105" ~ 105,
          Minute == "FreqHz_106" ~ 106,
          Minute == "FreqHz_107" ~ 107,
          Minute == "FreqHz_108" ~ 108,
          Minute == "FreqHz_109" ~ 109,
          Minute == "FreqHz_110" ~ 110,
          Minute == "FreqHz_111" ~ 111,
          Minute == "FreqHz_112" ~ 112,
          Minute == "FreqHz_113" ~ 113,
          Minute == "FreqHz_114" ~ 114,
          Minute == "FreqHz_115" ~ 115,
          Minute == "FreqHz_116" ~ 116,
          Minute == "FreqHz_117" ~ 117,
          Minute == "FreqHz_118" ~ 118,
          Minute == "FreqHz_119" ~ 119,
          Minute == "FreqHz_120" ~ 120,
          Minute == "FreqHz_121" ~ 121,
          Minute == "FreqHz_122" ~ 122,
          Minute == "FreqHz_123" ~ 123,
          Minute == "FreqHz_124" ~ 124,
          Minute == "FreqHz_125" ~ 125,
          Minute == "FreqHz_126" ~ 126,
          Minute == "FreqHz_127" ~ 127,
          Minute == "FreqHz_128" ~ 128,
          Minute == "FreqHz_129" ~ 129,
          Minute == "FreqHz_130" ~ 130,
          Minute == "FreqHz_131" ~ 131,
          Minute == "FreqHz_132" ~ 132,
          Minute == "FreqHz_133" ~ 133,
          Minute == "FreqHz_134" ~ 134,
          Minute == "FreqHz_135" ~ 135,
          Minute == "FreqHz_136" ~ 136,
          Minute == "FreqHz_137" ~ 137,
          Minute == "FreqHz_138" ~ 138,
          Minute == "FreqHz_139" ~ 139,
          Minute == "FreqHz_140" ~ 140,
          Minute == "FreqHz_141" ~ 141,
          Minute == "FreqHz_142" ~ 142,
          Minute == "FreqHz_143" ~ 143,
          Minute == "FreqHz_144" ~ 144,
          Minute == "FreqHz_145" ~ 145,
          Minute == "FreqHz_146" ~ 146,
          Minute == "FreqHz_147" ~ 147,
          Minute == "FreqHz_148" ~ 148,
          Minute == "FreqHz_149" ~ 149,
          Minute == "FreqHz_150" ~ 150,
          Minute == "FreqHz_151" ~ 151,
          Minute == "FreqHz_152" ~ 152,
          Minute == "FreqHz_153" ~ 153,
          Minute == "FreqHz_154" ~ 154,
          Minute == "FreqHz_155" ~ 155,
          Minute == "FreqHz_156" ~ 156,
          Minute == "FreqHz_157" ~ 157,
          Minute == "FreqHz_158" ~ 158,
          Minute == "FreqHz_159" ~ 159,
          Minute == "FreqHz_160" ~ 160,
          Minute == "FreqHz_161" ~ 161,
          Minute == "FreqHz_162" ~ 162,
          Minute == "FreqHz_163" ~ 163,
          Minute == "FreqHz_164" ~ 164,
          Minute == "FreqHz_165" ~ 165,
          Minute == "FreqHz_166" ~ 166,
          Minute == "FreqHz_167" ~ 167,
          Minute == "FreqHz_168" ~ 168,
          Minute == "FreqHz_169" ~ 169,
          Minute == "FreqHz_170" ~ 170,
          Minute == "FreqHz_171" ~ 171,
          Minute == "FreqHz_172" ~ 172,
          Minute == "FreqHz_173" ~ 173,
          Minute == "FreqHz_174" ~ 174,
          Minute == "FreqHz_175" ~ 175,
          Minute == "FreqHz_176" ~ 176,
          Minute == "FreqHz_177" ~ 177,
          Minute == "FreqHz_178" ~ 178,
          Minute == "FreqHz_179" ~ 179,
          Minute == "FreqHz_180" ~ 180
        )
    )
  return(df)
}

#plot components-------

#plot the mean as a point and the standard error bars
#make them red
geom_mean = function(positionWidth) {
  list(
    stat_summary(
      fun = mean, 
      geom = "point", 
      colour = "red", 
      size = 2, 
      position = position_dodge(width = positionWidth), 
      show.legend = FALSE
    ),
    stat_summary(
      geom = "errorbar", 
      fun.data = mean_se, 
      position = position_dodge(width = positionWidth), 
      width = 0.2, 
      colour = "red", 
      show.legend = FALSE
    )
  )
}

#make a dotplot layer
my_dot_geom = function(dotsize, binwidth, positionWidth){
  geom_dotplot(
    binaxis = "y", 
    stackdir = "center", 
    dotsize = dotsize, 
    binwidth = binwidth, 
    alpha = 0.7,
    position = position_dodge(width = positionWidth)
  )
}

#make a violin plot layer
my_violin_geom = function(positionWidth){
  geom_violin(
    alpha = 0.05, #make it mostly transparent
    color = "grey", #always make the color grey
    position = position_dodge(width = positionWidth)
  )
}

#make a dodge jitter plot
my_jitterDodge_geom = function(dotsize, jitterWidth, positionWidth){
  geom_point(
    size = dotsize, 
    alpha = 0.6, 
    position = position_jitterdodge(
      jitterWidth, #how much to jitter the dots
      dodge.width = positionWidth #alignment with other parts of the graph
    )
  )
}

#if the supplied ytitle is null, use the KNDy_VarNames data frame to find the nice name, otherwise use the supplied name
#Change this to "titleFunc" so that can be used more logically for x grouping as well
yTitleFunc = function(
  ytitle = NULL, 
  var_to_plot
){
  if(is.null(ytitle)){
    ytitle = KNDy_VarNames[as.character(var_to_plot)]
  }else{ytitle = ytitle}
}

#function for all of the layers

my_KNDy_geoms = function(
  dotsize, 
  binwidth, #relevant for dotplot
  positionWidth,
  xtitle, #for x axis nice title
  ytitle, #for y axis nice title
  var_to_plot, 
  title, #title of the graph
  expand_to_zero, #expand y-axis to zero, if desired -> TRUE
  dot_plot, #if want to add dot plot lyaer -> TRUE
  violin_plot, #if want to add violin layer -> TRUE
  zoom_y = FALSE,
  #1/1/2021 - issues with min/max being null. Still get occassional errors with NA, but they at least resolve themselves
  #get practice may be to always provide both min and max, and not leave either null
  ylimit = 20, #if want to zoom y axis, set upper y limit
  mean_plot = TRUE, #if want to include mean and SEM
  ymin = 0
){
  list(
    labs(
      x = xtitle, 
      y = yTitleFunc(ytitle, var_to_plot), 
      title = title
    ),
    if(dot_plot) #if dot_plot is true, add this layer
      my_dot_geom(dotsize, binwidth, positionWidth),
    if (violin_plot) #if violin_plot is true, add this layer
      my_violin_geom(positionWidth),
    if(mean_plot)
      geom_mean(positionWidth),
    if(expand_to_zero) #if expand_to_zero is true
      expand_limits(y=0), #set y axis to 0
    if(zoom_y)
      coord_cartesian(ylim = c(ymin, ylimit)), #changes so that it just "zooms" the graph, without changing the data that is considered for summaries
    my_theme
  )
}


#use a file prefix, the plot type, the plotting variable, and any addition "add_to_save_name" to create a file name
#saves a png file
#General structure [file_prefix]_[plot_type]_[var_to_plot]_[Date]_[add_to_save_name].png
naming_plot = function(file_prefix, plot_type, var_to_plot, add_to_save_name, img_type = ".png"){
  paste0(
    file_prefix, 
    "_",
    plot_type,
    "_",
    as_name(var_to_plot),
    "_", 
    Sys.Date(),
    add_to_save_name, 
    img_type
  )
}

#saving function with transparent background and to the PlotOutputFolder
#supply the basename, plotting variable, and addtional appendix for the naming_PNG_plot function
#supply the plot to be saved
#additional arguments allowed (...), including width/height
my_ggsave = function(
  plot_type,
  var_to_plot, #append to basename
  add_to_save_name, #append additional info
  plot, #plot to be saved
  img_type = ".png", #type of image to save
  path = PlotOutputFolder, #PlotOutputFolder defined in set up and .Renviron
  ...
){
  plot_name = naming_plot(
    file_prefix = file_prefix, 
    plot_type = plot_type,
    var_to_plot = var_to_plot, 
    add_to_save_name = add_to_save_name,
    img_type = img_type
  )  #see above
  ggsave(
    plot_name, 
    plot = plot, 
    path = path, 
    bg = "transparent", 
    units = "in", ...
  )
}


#add a slide with a graph to a powerpoint file
ppt_GraphFunc = function(ppt, viz){
  #add a new slide
  ppt = add_slide(
    ppt, 
    layout = "Blank", #use the blank layout
    master = "Office Theme"
  )
  ppt = ph_with(
    ppt, 
    value = viz, #add the visualization to the powerpoint
    location = ph_location_fullsize() #plot it on the entire slide
  )
}

ppt_GraphFunc_flag = function(ppt, viz, flag, cellID, genTreatment, ageGroup){
  #add a new slide
  ppt = add_slide(
    ppt, 
    layout = "Blank", #use the blank layout
    master = "Office Theme"
  )
  ppt = ph_with(
    ppt, 
    value = viz, #add the visualization to the powerpoint
    location = ph_location_fullsize() #plot it on the entire slide
  )
  
  ageGroupText = if(ageGroup == "Adult"){" adult"} else if(ageGroup == "Juvenile"){" juvenile"}
  
  ppt = ph_with(
    ppt, 
    value = block_list(
      fpar(
        ftext(cellID, prop = fp_text(font.size = 20))
        ),
      fpar(
        ftext(genTreatment,  prop = fp_text(font.size = 20)),
        ftext(ageGroupText,  prop = fp_text(font.size = 20))
      ),
      fpar(
        ftext(flag, prop = fp_text(font.size = 20))
      )
    ),
    location = ph_location(left = 7, width = 3)
  )
}

#### Main KNDy plot function
#Use the above components to create and save the graph as desired
#Can add a dot_plot layer (bins the dots)
#Can add a violin_plot
#Also plots the mean and standard error 

#Specify the dotsize and binwidth - if the binwidth is really small, then need a bigger dot size

#If do not specify y-title, then it plots with the "nice name" on the y-axis. 
#Default is no title

#Can choose to save directly with this function, and can add extra info to name, otherwise name will be
#"AG_KNDyPNA_plot_[var]_[date].png"

my_KNDy_plot_fill = function(
  data, #data frame
  var_to_plot, #which variable to plot
  dotsize, 
  binwidth,
  positionWidth = 0.9, #dodge position
  usefill = TRUE, #separate by another variable in addition to treatment
  fill = AgeGroup, #Default separate by AgeGroup
  treatment = GenTreatment, #which treatment variable to use. Default to general treatment (PNA/Control)
  ytitle = NULL, #y axis label
  title = NULL, #title
  expand_to_zero = TRUE, #expand axis to 0 
  save = FALSE, #Save png
  add_to_save_name = NULL, #append something additional to file name
  dot_plot = TRUE, #use the dot_plot layer
  violin_plot = FALSE, #add the violin plot layer
  toPPT = FALSE, #add to a powerpoint
  ppt = NULL, #powerpoint object to add to
  zoom_y = FALSE,
  ylimit = NULL,
  img_type = ".png",
  figWidth = 10,
  figHeight = NA
){
  var_to_plot = ensym(var_to_plot)
  treatment = ensym(treatment)
  
  if(usefill == TRUE){ #if using fill
    fill = ensym(fill)
  }else{fill = NULL} #otherwise, fill is NULL
  
  viz = data %>%
    ggplot(aes(x = !! treatment, y = !! var_to_plot, fill = !! fill))+
    my_KNDy_geoms(
      dotsize = dotsize, 
      binwidth = binwidth, 
      positionWidth = positionWidth, 
      xtitle = "Group", 
      ytitle = ytitle, 
      var_to_plot = var_to_plot, 
      title = title, 
      expand_to_zero = expand_to_zero, 
      dot_plot = dot_plot,
      violin_plot = violin_plot,
      zoom_y = zoom_y,
      ylimit = ylimit
    )
  
  if(save){ #if save is true
    my_ggsave(
      plot_type = "plot", 
      var_to_plot = var_to_plot, 
      add_to_save_name = add_to_save_name, 
      plot = viz, 
      width = figWidth,
      height = figHeight,
      img_type = img_type
    )
  }
  
  if(toPPT){ #if toPPT is true
    ppt_GraphFunc(ppt, viz)
  }
  return(viz) #return the viz plot
}

#switch so that treatment is the fill variable. Group on x by another variable
my_KNDy_plot = function(
  data, #data frame
  var_to_plot, #which variable to plot
  dotsize, 
  binwidth,
  positionWidth = 0.9, #dodge position
  group = AgeGroup, #Default separate by AgeGroup
  treatment = GenTreatment, #which treatment variable to use. Default to general treatment (PNA/Control)
  ytitle = NULL, #y axis label
  title = NULL, #title
  expand_to_zero = TRUE, #expand axis to 0 
  save = FALSE, #Save png
  add_to_save_name = NULL, #append something additional to file name
  dot_plot = TRUE, #use the dot_plot layer
  violin_plot = FALSE, #add the violin plot layer
  toPPT = FALSE, #add to a powerpoint
  ppt = NULL, #powerpoint object to add to
  zoom_y = FALSE,
  ylimit = NULL,
  img_type = ".png", #type of image to save
  figWidth = 10,
  figHeight = NA
){
  var_to_plot = ensym(var_to_plot)
  treatment = ensym(treatment)
  group = ensym(group)
  
  viz = data %>%
    ggplot(aes(x = !! group, y = !! var_to_plot, fill = !! treatment))+
    my_KNDy_geoms(
      dotsize = dotsize, 
      binwidth = binwidth, 
      positionWidth = positionWidth, 
      xtitle = NULL, 
      ytitle = ytitle, 
      var_to_plot = var_to_plot, 
      title = title, 
      expand_to_zero = expand_to_zero, 
      dot_plot = dot_plot,
      violin_plot = violin_plot,
      zoom_y = zoom_y,
      ylimit = ylimit
    )
  
  if(save){ #if save is true
    my_ggsave(
      plot_type = "plot", 
      var_to_plot = var_to_plot, 
      add_to_save_name = add_to_save_name, 
      plot = viz, 
      width = figWidth,
      height = figHeight,
      img_type = img_type
    )
  }
  
  if(toPPT){ #if toPPT is true
    ppt_GraphFunc(ppt, viz)
  }
  return(viz) #return the viz plot
}

#Geoms to make a dodge jitter plot
my_KNDy_jitter_geoms = function(
  xtitle, 
  ytitle, 
  var_to_plot, 
  title, 
  dotsize, 
  jitterWidth, 
  positionWidth, 
  jitter_plot, 
  violin_plot
){
  list(
    labs(
      x = xtitle, 
      y = yTitleFunc(ytitle, var_to_plot), 
      title = title
    ),
    if(jitter_plot)
      geom_point(
        size = dotsize, 
        alpha = 0.6, 
        position = position_jitterdodge(
          jitterWidth, 
          dodge.width = positionWidth
        )
      ),
    geom_mean(positionWidth),
    if(violin_plot)
      my_violin_geom(positionWidth),
    my_theme
  )
}

#Actual function to plot the jitter dodge plot
my_KNDy_jitterDodge_plot = function(
  data, 
  var_to_plot,
  dotsize,
  positionWidth = 0.9,
  jitterWidth = 0.3,
  usefill = TRUE,
  fill = AgeGroup,
  treatment = GenTreatment,
  ytitle = NULL, 
  title = NULL, 
  expand_to_zero = TRUE, 
  save = FALSE, 
  add_to_save_name = NULL,
  jitter_plot = TRUE,
  violin_plot = FALSE,
  img_type = ".png")
{
  var_to_plot = ensym(var_to_plot)
  treatment = ensym(treatment)
  
  if(usefill == TRUE){
    fill = ensym(fill)
  }else{fill = NULL}
  
  viz = data %>%
    ggplot(aes(x = !! treatment, y = !! var_to_plot, fill = !! fill, color = !! fill))+
    my_KNDy_jitter_geoms(
      xtitle = "Group", 
      ytitle = ytitle, 
      var_to_plot = var_to_plot, 
      title = title, 
      dotsize = dotsize, 
      jitterWidth = jitterWidth, 
      positionWidth = positionWidth, 
      jitter_plot = jitter_plot, 
      violin_plot = violin_plot
    )
  
  if(save){
    my_ggsave(
      plot_type = "jitter", 
      var_to_plot = var_to_plot, 
      add_to_save_name = add_to_save_name, 
      plot = viz, 
      width = 10,
      img_type = img_type
    )
  }
  
  return(viz)
}

#plot by another variable
my_KNDy_scatter_plot = function(
  var_to_plot, 
  xaxis,
  data,
  add_to_save_name = NULL, 
  save = FALSE, 
  toPPT = FALSE, 
  ppt = NULL,
  img_type = ".png"
) {
  xaxis = ensym(xaxis)
  var_to_plot = ensym(var_to_plot)
  
  renaming = KNDy_VarNames[, as.character(var_to_plot)]
  
  viz = data %>%
    ggplot(aes(x = !! xaxis, y = !! var_to_plot, colour = GenTreatment))+
    geom_point(size = 3)+
    geom_smooth(method = lm, se = FALSE)+
    labs(
      x = KNDy_VarNames[, as.character(xaxis)], 
      y = renaming, 
      title = renaming, 
      subtitle = "By Firing Rate", 
      fill = "Treatment"
    )+
    my_theme
  
  plotBy = paste0("by_", as.character(xaxis))
  
  if(save == TRUE){
    my_ggsave(
      plot_type = plotBy, 
      var_to_plot = var_to_plot, 
      add_to_save_name = add_to_save_name, 
      plot = viz, 
      width = 9, 
      height = 6,
      img_type = img_type
    )
  }
  
  if(toPPT){
    ppt_GraphFunc(ppt, viz)
  }
  
  return(viz)
}


#Line Geom ------
my_geom_line <- function(
  linetype_var, #use expr
  lineGroup_var #use expr
){
  geom_line(
    alpha = .25, #make it semi-transparent
    aes(linetype = !! linetype_var,
        group = !! lineGroup_var),
    size = 0.8
  )
}

#plot the average as a solid line. Does not include error bars
#Can use a different line type based on a categorical variable
#Mean line geom ------------
my_line_mean_geom <- function(
  useLinetype, #TRUE/FALSE
  linetype_var #use expr
){
  list(
    stat_summary(fun = mean, geom = "line", if(useLinetype){aes(linetype = !! linetype_var)}, size = 1.4, alpha = 1)
    #stat_summary(geom = "errorbar", fun.data = mean_se, size = 1)
  )
}

#Geoms to make a line plot for VBW -----------
my_KNDy_VBW_geoms = function(
  useLinetype, #TRUE/FALSE
  linetype_var, #use expr
  lineGroup_var, #use expr
  xtitle, #for x axis nice title
  ytitle, #for y axis nice title
  title = NULL, #title of the graph
  individualLines = TRUE, #if want individual lines
  mean_lines = TRUE, #if want to include mean line with SEM
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL
){
  list(
    labs(
      x = xtitle, 
      y = ytitle, 
      title = title
    ),
    if(individualLines) #if individualLines is true, add this layer
      my_geom_line(
        linetype_var = linetype_var,
        lineGroup_var = lineGroup_var
      ),
    if(mean_lines) #if mean_lines is true, add this layer
      my_line_mean_geom(
        useLinetype = useLinetype,
        linetype_var = linetype_var
      ),
    expand_limits(y=0), #set y axis to 0
    coord_cartesian( #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
      if(zoom_x){xlim = c(xmin, xmax)}, 
      if(zoom_y){ylim = c(ymin, ymax)}
    ), 
    my_theme
  )
}

#Plots Lines function - VBW ----------------
VBW_plot_lines = function(
  df, 
  line_group = expr(CellID), 
  individualLines = TRUE, #if want individual lines
  mean_lines = TRUE, #if want to include mean line with SEM
  title = NULL,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL)
{
  ggplot(
    df, 
    aes(
      BW_msec, 
      BurstsPerHour, 
      color = GenTreatment, 
      group = interaction(GenTreatment, AgeGroup)
    )
  ) + 
    my_KNDy_VBW_geoms(
      useLinetype = TRUE,
      linetype_var = expr(AgeGroup),
      lineGroup_var = line_group,
      xtitle = "Burst Window (msec)",
      ytitle = "Bursts per Hour",
      title = title,
      individualLines = individualLines,
      mean_lines = mean_lines,
      zoom_x = zoom_x,
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y,
      ymin = ymin,
      ymax = ymax
    )
}

#Cycles Plot --------
cyclesPlotFunc <- function(df){
  ggplot(df, aes(x = Day, y = Stage)) +
    geom_line() +
    my_theme +
    facet_wrap(Mouse_ID ~ .) + #each plot is a mouse
    scale_y_continuous(
      breaks = c(1, 2, 3), #axis ticks only at 1, 2, 3
      labels = c("E", "D", "P") #replace with E, D, and P
    ) +
    scale_x_continuous(
      breaks = seq(1, 21, 3) #labels every third integer
    )
}

#Firing Rate Plot --------
firingRatePlotFunc <- function(
  df, 
  zoom_x = FALSE,
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE,
  ymin = NULL,
  ymax = NULL,
  excludeLineType = TRUE, # changes line-type based on whether or not cell is marked for exclusion
  save = FALSE, #Save png
  add_to_save_name = NULL, #append something additional to file name
  toPPT = FALSE, #add to a powerpoint
  ppt = NULL, #powerpoint object to add to
  img_type = ".png",
  figWidth = 10,
  figHeight = NA,
  cellID = "",
  addFlag = FALSE,
  flagText = NULL,
  genTreatment = NULL,
  ageGroup = NULL
){
  viz <- ggplot(df, aes(x = Min_num, y = FiringRate_Hz, color = interaction(Who, WhoRecorded))) +
    geom_line(
      if(excludeLineType){aes(linetype = Exclude)}
    ) +
    my_theme +
    facet_wrap(CellID ~ WhoRecorded, ncol = 3) + #each plot is a cell
    scale_x_continuous(
      breaks = seq(0, 180, 15) #labels every 15 minutes
    )+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) + #this just zooms in on the graph, versus scale_[]_continuous actually eliminates data not in the range
    labs(x = "Time (min)", y = "Firing Rate (Hz)") + 
    scale_color_manual(
      values = c("Amanda.Amanda" = "orange", "Jenn.Amanda" = "red", "Jenn.Jenn" = "blue", "Amanda.Jenn" = "lightblue"),
      breaks = c("Amanda.Amanda", "Jenn.Amanda", "Jenn.Jenn", "Amanda.Jenn"),
      labels = c("Amanda slice + record", "Jenn slice; Amanda record", "Jenn slice + record", "Amanda slice; Jenn record")
      )
  
  if(excludeLineType){
    viz <- viz +
      scale_linetype_manual(
        values = c("FALSE" = "solid", "TRUE" = "dotted"),
        breaks = c("FALSE", "TRUE"),
        labels = c("Included", "Excluded")
      )
  }
  
  if(save){ #if save is true
    my_ggsave(
      plot_type = "firingRate", 
      var_to_plot = cellID, 
      add_to_save_name = add_to_save_name, 
      plot = viz, 
      width = figWidth,
      height = figHeight,
      img_type = img_type,
      path = file.path(PlotOutputFolder, "firingRate")
    )
  }
  
  if(toPPT && addFlag){ #if toPPT is true
    ppt_GraphFunc_flag(ppt, viz, flagText, cellID, genTreatment, ageGroup)
  }else if(toPPT){
    ppt_GraphFunc(ppt, viz)
  }
  
  return(viz) #return the viz plot
}

# To plot a single cell
firingRatePlot_SingleCellFunc <- function(
  cellID,
  df,
  zoom_x = FALSE,
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE,
  ymin = NULL,
  ymax = NULL,
  excludeLineType = TRUE, # changes line-type based on whether or not cell is marked for exclusion
  save = FALSE, #Save png
  zoomInfo = NULL, #append something additional to file name
  toPPT = FALSE, #add to a powerpoint
  ppt = NULL, #powerpoint object to add to
  img_type = ".png",
  figWidth = 10,
  figHeight = NA,
  addFlag = FALSE,
  flagText = NULL,
  genTreatment = NULL,
  ageGroup = NULL
){
  if(is.na(flagText)) {
    flagText = ""
  }
  # if(is.na(flagText)) {
  #   addFlag = FALSE
  # }
  df %>%
    filter(CellID == cellID) %>%
    firingRatePlotFunc(
      zoom_x = zoom_x,
      xmin = xmin,
      xmax = xmax,
      zoom_y = zoom_y,
      ymin = ymin,
      ymax = ymax,
      excludeLineType = excludeLineType,
      save = save,
      add_to_save = zoomInfo,
      toPPT = toPPT,
      ppt = ppt,
      img_type = img_type,
      cellID = cellID,
      addFlag = addFlag,
      flagText = flagText,
      genTreatment = genTreatment,
      ageGroup = ageGroup
    )
}

# To plot a single cell
firingRatePlot_SingleCellFunc_1_20 <- function(
  cellID,
  df,
  excludeLineType = TRUE, # changes line-type based on whether or not cell is marked for exclusion
  save = FALSE, #Save png
  toPPT = FALSE, #add to a powerpoint
  ppt = NULL, #powerpoint object to add to
  img_type = ".png",
  figWidth = 10,
  figHeight = NA,
  addFlag = FALSE,
  flagText = NULL,
  genTreatment = NULL,
  ageGroup = NULL
){
  if(is.na(flagText)) {
    flagText = ""
  }
  df %>%
    filter(CellID == cellID) %>%
    firingRatePlotFunc(
      zoom_x = TRUE,
      xmin = 0,
      xmax = 180,
      zoom_y = TRUE,
      ymin = 0,
      ymax = 1,
      excludeLineType = excludeLineType,
      save = save,
      add_to_save = "_1Hz",
      toPPT = toPPT,
      ppt = ppt,
      img_type = img_type,
      cellID = cellID,
      addFlag = addFlag,
      flagText = flagText,
      genTreatment = genTreatment,
      ageGroup = ageGroup
    )
  
  df %>%
    filter(CellID == cellID) %>%
    firingRatePlotFunc(
      zoom_x = TRUE,
      xmin = 0,
      xmax = 180,
      zoom_y = TRUE,
      ymin = 0,
      ymax = 20,
      excludeLineType = excludeLineType,
      save = save,
      add_to_save = "_20Hz",
      toPPT = toPPT,
      ppt = ppt,
      img_type = img_type,
      cellID = cellID,
      addFlag = addFlag,
      flagText = flagText,
      genTreatment = genTreatment,
      ageGroup = ageGroup
    )
}



### Drafts - not using currently ---------------

# my_KNDy_jitter_by_sac_time = function(data, var_to_plot, ytitle = NULL, title = NULL){
#   var_to_plot = enquo(var_to_plot)
#   viz = data %>%
#     ggplot(aes(x = GenTreatment, y = !! var_to_plot))+
#     geom_jitter(width = .2, size = 3, alpha = .6, aes(colour = Sac_9plus))+
#     stat_summary(fun.y = mean, geom = "point") +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
#     my_theme+
#     theme(legend.title = element_text())+
#     expand_limits(y = 0)+
#     scale_colour_manual(values = c("royalblue4", "tomato4"), name = "Sac Time (rel. to lights on)", labels = c("<9hr", ">9hr"))
#   #scale_colour_discrete(name = "Sac'd 9h after lights on", labels = c("No", "Yes"))
#   
#   if(is.null(ytitle)){
#     viz = viz
#   }else{viz = viz + labs(y = ytitle)}
#   
#   if(is.null(title)){
#     viz = viz
#   }else{viz = viz + labs(title = title)}
#   return(viz)
# }
# 
# my_KNDy_jitter2 = function(data, var_to_plot, ytitle = NULL, title = NULL){
#   var_to_plot = enquo(var_to_plot)
#   viz = data %>%
#     ggplot(aes(x = GenTreatment, y = !! var_to_plot))+
#     geom_violin(alpha = 0.5, color = "grey")+
#     geom_jitter(width = .05, size = 3, alpha = .6, color = "royalblue4")+
#     stat_summary(fun.y = mean, geom = "point") +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.3)+
#     my_theme+
#     theme(legend.title = element_text())+
#     expand_limits(y = 0)
#   
#   if(is.null(ytitle)){
#     viz = viz
#   }else{viz = viz + labs(y = ytitle)}
#   
#   if(is.null(title)){
#     viz = viz
#   }else{viz = viz + labs(title = title)}
#   return(viz)
# }


# my_KNDy_dotplot_by_treatment2 = function(data, var_to_plot, ytitle = NULL, title = NULL, expand_to_zero = TRUE, save = FALSE){
#   var_to_plot = ensym(var_to_plot)
#   renaming = KNDy_VarNames[as.character(var_to_plot)]
#   plot_name = paste0("KNDyDotplot_", as_name(var_to_plot), "_", Sys.Date(), ".png")
#   viz = data %>%
#     ggplot(aes(x = GenTreatment, y = !! var_to_plot))+
#     geom_violin(alpha = 0.5, color = "grey")+
#     geom_dotplot(binaxis = "y", stackdir = "center", dotsize = .75)+
#     stat_summary(fun.y = mean, geom = "point", colour = "red", size = 2) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, colour = "red")+
#     my_theme
#   
#   if(is.null(ytitle)){
#     viz = viz + labs(y = renaming, x = "Treatment")
#   }else{viz = viz + labs(y = ytitle, x = "Treatment")}
#   
#   if(is.null(title)){
#     viz = viz
#   }else{viz = viz + labs(title = title)}
#   
#   if(expand_to_zero == FALSE){
#     if(save == TRUE){
#       ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent")
#     }
#     return(viz)
#   }else{
#     viz = viz + expand_limits(y = 0) #set y axis to 0
#     if(save == TRUE){
#       ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent")
#     }
#     return(viz) 
#   }

####OLD-----------------------------------------------------
# #This allows you to group a df by an unspecified number of variables
# grouping_by = function(df, ...){
#   group_vars = quos(...)
#   df = df %>%
#     group_by(!!! group_vars)
# }
# 
# #Firing Rate Equations
# #Can be used to create summaries based on particular variables
# summary_by_var = function(var){
#   var = enquo(var)
#   function(df){
#     df = df %>%
#       summarize(Mean = mean(!! var, na.rm = TRUE),
#                 SD = sd(!! var, na.rm = TRUE),
#                 n = n(),
#                 SEM = SD/sqrt(n))
#     return(df)
#   }
# }
# 
# #from my_functions: data[,variable]. Not sure if can feed multiple variables into this
# 
# #Equation to Summarize Average Spontaneous Firing Rate
# FiringRate_sum = summary_by_var(SpontAvgFiring)
# 
# FiringRate_sum_byGroup = function(df, ...){
#   group_vars = quos(...)
#   df = df %>%
#     group_by(!!! group_vars) %>%
#     FiringRate_sum()
# }
# 
# summary_byVar_byGroup = function(var_toSummarize, df, ...){
#   var_toSummarize = enquo(var_toSummarize)
#   group_vars = quos(...)
#   df = df %>%
#     group_by(!!! group_vars) %>%
#     summarize(Mean = mean(!! var_toSummarize, na.rm = TRUE),
#               SD = sd(!! var_toSummarize, na.rm = TRUE),
#               n = n(),
#               SEM = SD/sqrt(n))
#   return(df)
# }

# my_KNDy_jitter_plot = function(var_to_plot){
#   var_to_plot = enquo(var_to_plot)
#   viz = KNDyDATA %>%
#     ggplot(aes(x = GenTreatment, y = !! var_to_plot))+
#     geom_jitter(width = .1)+
#     geom_mean(1)+
#     my_theme
#   return(viz)
# }


# #This basically bins the y-values and plots a dot histogram
# my_KNDy_dotplot = function(var_to_plot, expand_to_zero = TRUE, save = FALSE){
#   var_to_plot = ensym(var_to_plot)
#   renaming = KNDy_VarNames[, as.character(var_to_plot)]
#   plot_name = paste0("KNDyDotplot_", as.character(var_to_plot), "_", Sys.Date(), ".png")
#   viz = 
#     ggplot(KNDyDATA, aes(x = GenTreatment, y = !! var_to_plot))+
#     geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.75)+
#     stat_summary(fun.y = mean, geom = "point", colour = "red", size = 2) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.2, colour = "red")+
#     labs(y = renaming, title = renaming, x = KNDy_VarNames[,"GenTreatment"])+
#     #theme_classic()+
#     theme(
#       text = element_text(size=20),
#       panel.background = element_rect(fill = "transparent"), # bg of the panel
#       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#       panel.grid.major = element_blank(), # get rid of major grid
#       panel.grid.minor = element_blank(), # get rid of minor grid
#       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#       legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
#       axis.line = element_line(colour = "black")
#     )
#   if(expand_to_zero == FALSE){
#     if(save == TRUE){
#       ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent")
#     }
#     return(viz)
#   }else{
#     viz = viz + expand_limits(y = 0) #set y axis to 0
#     if(save == TRUE){
#       ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent")
#     }
#     return(viz) 
#   }
#   
# }
# 
# #plot by age
# plot_by_age = function(var_to_plot) {
#   var_to_plot = ensym(var_to_plot)
#   renaming = KNDy_VarNames[, as.character(var_to_plot)]
#   plot_name = paste0("KNDy_by_age_", as.character(var_to_plot), "_", Sys.Date(), ".png")
#   viz = KNDyDATA %>%
#     ggplot(aes(x = Age_in_days, y = !! var_to_plot))+
#     geom_point(aes(colour = GenTreatment), size = 3)+
#     labs(x = KNDy_VarNames[, "Age_in_days"], y = renaming, title = renaming, subtitle = "By Age", fill = "Treatment")+
#     my_theme
#   
#   ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent", width = 9, height = 6)
#   return(viz)
# }
# 
# plot_by_sactime = function(data, var_to_plot, save = FALSE){
#   var_to_plot = ensym(var_to_plot)
#   renaming = KNDy_VarNames[, as.character(var_to_plot)]
#   plot_name = paste0("KNDy_by_time_sac_", as.character(var_to_plot), "_", Sys.Date(), ".png")
#   viz = data %>%
#     ggplot(aes(x = Sac_hr, y = !! var_to_plot, colour = GenTreatment))+
#     geom_point(size = 3)+
#     geom_smooth(method = lm, se = FALSE)+
#     labs(x = KNDy_VarNames[, "Sac_hr"], y = renaming, title = renaming, subtitle = "By Time of Sacrifice", fill = "Treatment")+
#     #theme_classic()+
#     theme(
#       text = element_text(size=18),
#       legend.title = element_blank(),
#       legend.position = "bottom",
#       panel.background = element_rect(fill = "transparent"), # bg of the panel
#       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#       panel.grid.major = element_blank(), # get rid of major grid
#       panel.grid.minor = element_blank(), # get rid of minor grid
#       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#       legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
#       axis.line = element_line(colour = "black")
#     )
#   if(save == TRUE){
#     ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent", width = 9, height = 6)
#   }
#   return(viz)
# }
# 
# plot_by_recordtime = function(data, var_to_plot, save = FALSE){
#   var_to_plot = ensym(var_to_plot)
#   renaming = KNDy_VarNames[, as.character(var_to_plot)]
#   plot_name = paste0("KNDy_by_record_time_", as.character(var_to_plot), "_", Sys.Date(), ".png")
#   viz = data %>%
#     ggplot(aes(x = Record_start_hr, y = !! var_to_plot, colour = GenTreatment))+
#     geom_point(size = 3)+
#     geom_smooth(method = lm, se = FALSE)+
#     labs(x = KNDy_VarNames[, "Record_start_hr"], y = renaming, title = renaming, subtitle = "By Time of Recording", fill = "Treatment")+
#     #theme_classic()+
#     theme(
#       text = element_text(size=18),
#       legend.title = element_blank(),
#       legend.position = "bottom",
#       panel.background = element_rect(fill = "transparent"), # bg of the panel
#       plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
#       panel.grid.major = element_blank(), # get rid of major grid
#       panel.grid.minor = element_blank(), # get rid of minor grid
#       legend.background = element_rect(fill = "transparent"), # get rid of legend bg
#       legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
#       axis.line = element_line(colour = "black")
#     )
#   if(save == TRUE){
#     ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent", width = 9, height = 6)
#   }
#   return(viz)
# }

# #This uses a dotplot (it bins the y-values, and is set to center them) and a violin plot, 
# #Also plots the mean and standard error 
# #Specify the dotsize and binwidth - if the binwidth is really small, then need a bigger dot size
# #If do not specify y-title, then it plots with the "nice name" on the y-axis. Default is no title
# #Can choose to save directly with this function, and can add extra info to name, otherwise name will be
# #"KNDyViolin_[var]_[date].png"
# my_KNDy_violin = function(data, 
#                           var_to_plot,
#                           dotsize, 
#                           binwidth,
#                           usefill = TRUE,
#                           fill = AgeGroup,
#                           treatment = GenTreatment,
#                           ytitle = NULL, 
#                           title = NULL, 
#                           expand_to_zero = TRUE, 
#                           save = FALSE, 
#                           add_to_save_name = NULL){
#   var_to_plot = ensym(var_to_plot)
#   treatment = ensym(treatment)
#   
#   dodgePosition = position_dodge(width = .9)
#   
#   if(usefill == TRUE){
#     fill = ensym(fill)
#   }else{fill = NULL}
#   
#   renaming = KNDy_VarNames[as.character(var_to_plot)]
#   plot_name = paste0("KNDyViolin_", 
#                      as_name(var_to_plot), 
#                      "_", Sys.Date(), 
#                      add_to_save_name, ".png")
#   viz = data %>%
#     ggplot(aes(x = !! treatment, y = !! var_to_plot, fill = !! fill))+
#     geom_violin(alpha = 0.05, color = "grey", position = dodgePosition)+
#     geom_dotplot(binaxis = "y", stackdir = "center", 
#                  dotsize = dotsize, 
#                  binwidth = binwidth, 
#                  alpha = 0.7,
#                  position = dodgePosition,
#     )+
#     stat_summary(fun.y = mean, geom = "point", colour = "red", size = 2, position = dodgePosition, show.legend = FALSE) +
#     stat_summary(geom = "errorbar", fun.data = mean_se, position = dodgePosition, width = 0.2, colour = "red", show.legend = FALSE)+
#     labs(x = "Group")+
#     my_theme
#   
#   if(is.null(ytitle)){
#     viz = viz + labs(y = renaming)
#   }else{viz = viz + labs(y = ytitle)}
#   
#   if(! is.null(title)){
#     viz = viz + labs(title = title)
#   }
#   
#   if(expand_to_zero == TRUE){
#     viz = viz + expand_limits(y = 0) #set y axis to 0
#   }
#   
#   if(save == TRUE){
#     ggsave(plot_name, plot = viz, path = PlotOutputFolder, bg = "transparent", width = 10, units = "in")
#   }
#   return(viz)
# }

# myCSV_func = function(folderPath, fileName) {
#   fullPath = paste0(folderPath, fileName)
#   cat("\n", file = fullPath, append = TRUE) #in theory, this should add a blank line at the end and avoid the "incomplete final line" error
#   read.csv(fullPath, na.strings="NA", fileEncoding="UTF-8-BOM")
# }
