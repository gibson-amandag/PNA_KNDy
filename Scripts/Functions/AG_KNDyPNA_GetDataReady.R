#The function takes the mouse demo data frame and the cells data frame to produce a combined KNDyDATA data frame
#This does the following transformations
# - reformats the columns with dates into date format
# - Creates a new variable Sac_9plus that is TRUE when the time of sacrifice is more than 9hr after lights on
# - Creates a new variable AgeGroup that is TRUE when the mouse is a juvenile
# - Merges the KNDy_exclude DF with the KNDy_cells DF to add "Exclude" variable
# - Creates a new variable Quiet that is TRUE when the spontaneous average firing rate is less than a cutoff value
#cutoff value is specified by rateForQuiet
# - Creates a new variable TreatxAge to generate four groups 
#PNA - Adult, PNA - Juvenile, Control - Adult, Control - Juvenile
# - Makes AgeGroup a factor, and gives it nice labels, Juvenile or Adult
# - Gets rid of white space in the "Who" variable
# - Makes Treatment a factor variable with orders "Main Colony Control", "Control", "Vehicle", "DHT"
# - Readjusts the order of variables within the KNDyDATA data frame to be a little more helpful. 
# If the order of the excel sheet changes, this has to change

#Returns a list with the three data frames

GetDataReadyFunc = function(KNDy_mouse_demo, KNDy_cells, KNDy_exclude, KNDy_firingRate, rateForQuiet){
  #Reformat dates into year-month-day
  #I think not needed with reading of excel files
  # KNDy_mouse_demo$Date_of_birth = format_dates(KNDy_mouse_demo$Date_of_birth)
  # KNDy_mouse_demo$Recording_date = format_dates(KNDy_mouse_demo$Recording_date)
  
  dateToday = Sys.Date()
  
  #Create a new variable that is TRUE when the time of sacrifice is more than 9hr after lights on
  KNDy_mouse_demo = KNDy_mouse_demo %>%
    mutate(Sac_9plus = ifelse(Sac_hr >= 9, TRUE, FALSE))
  
  #Create a new variable that is TRUE when the mouse is a juvenile
  KNDy_mouse_demo = KNDy_mouse_demo %>%
    mutate(AgeGroup = ifelse(Age_in_days <=22, TRUE, FALSE))
  
  #Merge the KNDy_exclude DF to the KNDy_cells DF create an "Exclude" variable
  KNDy_cells = KNDy_cells %>%
    left_join(
      KNDy_exclude %>% #only adds to the KNDy_cells frame. If a cell doesn't exist there, doesn't add it
        select(-MouseID), #don't add or merge on this
      by = "CellID" #merge by CellID
    )
  
  #Add the demographic information to the KNDy_cell dataframe
  KNDyDATA = KNDy_cells %>%
    select(-Daylight_Savings)%>%
    left_join(KNDy_mouse_demo, by = "MouseID")
  
  #Mark cells that are quiescent; Firing rate of less than 0.001 Hz - May want to adjust
  KNDyDATA = KNDyDATA %>%
    mutate(Quiet = ifelse(SpontAvgFiring <= rateForQuiet, TRUE, FALSE))
  
  #Create a Variable to generate four groups - PNA - Adult, PNA - Juvenile, Control - Adult, Control - Juvenile
  KNDyDATA = KNDyDATA %>%
    mutate(
      TreatxAge = ifelse(
        GenTreatment == "Control" & AgeGroup == FALSE,
        "Con-Adult",
        ifelse(
          GenTreatment == "Control" & AgeGroup == TRUE,
          "Con-Juvenile",
          ifelse(
            GenTreatment == "PNA" & AgeGroup == FALSE,
            "PNA-Adult",
            ifelse(
              GenTreatment == "PNA" & AgeGroup == TRUE,
              "PNA-Juvenile", NA
            )
          )
        )
      )
    )
  
  #Make AgeGroup a factor, and give it nice labels
  KNDyDATA$AgeGroup = factor(
    KNDyDATA$AgeGroup, 
    levels = c(TRUE, FALSE), 
    labels = c("Juvenile", "Adult")
  )
  
  #Get rid of white space in the names
  KNDyDATA$Who = gsub('\\s+', '', KNDyDATA$Who)
  
  #Make "Who" recorded variable a factor
  KNDyDATA$Who = factor(KNDyDATA$Who, levels = c("Jenn", "Amanda"))
  
  #Make Treatment a factor variable with orders
  KNDyDATA$Treatment = factor(
    KNDyDATA$Treatment, 
    levels = c("CON_main", "CON", "VEH", "DHT"), 
    labels = c("Main Colony Control", "Control", "Vehicle", "DHT")
  )
  
  ##THIS IS IMPORTANT - IF COLUMNS CHANGE, NEED TO BE SURE THAT THAT CHANGE IS REFLECTED HERE##
  KNDyDATA = KNDyDATA %>%
    select(
      CellID:MouseID,
      Cage:GenTreatment, 
      Exclude, 
      AgeGroup, 
      Who, 
      Record_start:Flagged, 
      BodyMass_g:Sac_hr, 
      Sac_9plus, 
      Quiet, 
      TreatxAge, 
      CycleStage, 
      SpontAvgFiring:MaxBurstWindow_senktide, 
      BurstsPerHour_0.01:BurstsPerHour_spont_BW2
    )
  
  #Add demographic information
  KNDy_firingRate <- KNDy_firingRate %>%
    left_join(KNDyDATA %>% select(CellID:CycleStage), by = "CellID")
  
  my_list = list(
    "KNDyDATA" = KNDyDATA, 
    "KNDy_mouse_demo" = KNDy_mouse_demo, 
    "KNDy_cells" = KNDy_cells,
    "KNDy_firingRate" = KNDy_firingRate
  )
  
  return(my_list)
}
