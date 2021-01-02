#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#if not prompted directly, use the following to install these packages.
# You'll have to uncomment them (select and press cmd/ctrl + shift + c, or just erase the #)

# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("rlang")
# install.packages("purrr")
# install.packages("scales")
# install.packages("knitr")
# install.packages("officer")
# install.packages("GGally")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("car")
# install.packages("kableExtra")

source("./Scripts/AG_KNDyPNA_setup.R")

source(file.path(FunctionsFolder, GetDataReadyFileName))
#The function takes the mouse demo data frame and the cells data frame to produce a combined KNDyDATA data frame
#This does the following transformations
# - reformats the columns with dates into date format
# - Creates a new variable Sac_9plus that is TRUE when the time of sacrifice is more than 9hr after lights on
# - Creates a new variable AgeGroup that is TRUE when the mouse is a juvenile
# - Creates a new variable Quiet that is TRUE when the spontaneous average firing rate is less than a cutoff value
#cutoff value is specified by rateForQuiet
# - Creates a new variable TreatxAge to generate four groups 
#PNA - Adult, PNA - Juvenile, Control - Adult, Control - Juvenile
# - Makes AgeGroup a factor, and gives it nice labels, Juvenile or Adult
# - Gets rid of white space in the "Who" variable
# - Makes Treatment a factor variable with orders "Main Colony Control", "Control", "Vehicle", "DHT"
# - Readjusts the order of variables within the KNDyDATA data frame to be a little more helpful. 
# If the order of the excel sheet changes, this has to change

allDFs = GetDataReadyFunc(KNDy_mouse_demo, KNDy_cells, KNDy_exclude, KNDy_firingRate, rateForQuiet)

#<<- assigns at a higher environment level
KNDyDATA <<- allDFs$KNDyDATA
KNDy_mouse_demo <<- allDFs$KNDy_mouse_demo
KNDy_cells <<- allDFs$KNDy_cells
KNDy_firingRate2 <<- allDFs$KNDy_firingRate

#create juvenile and adult dataframes
sepDFs <- sep_by_age(KNDyDATA)
KNDyDATA_juv <<- sepDFs$df_juv
KNDyDATA_adult <<- sepDFs$df_adult

AppScriptsFolder = file.path(ScriptsFolder, "app_scripts")

sourceModule <- function(moduleName){
  source(file.path(AppScriptsFolder, moduleName))
}

#Modules
#summary plot instructions
sourceModule("instructionsSummaryModule.R")
#scatter plot instructions
sourceModule("instructionsScatterModule.R")
#summary table instructions
sourceModule("instructionsSumTableModule.R")
#panel 4 instructions
sourceModule("panel4_instructions.R")


sourceModule("compBurstWindowModule.R")
sourceModule("selectDataModule.R")
sourceModule("zoomAxisModule.R")
sourceModule("controlDotsModule.R")
sourceModule("burstParamsModule.R")
sourceModule("cyclesModule.R")
sourceModule("firingRateModule.R")

sourceModule("summaryPlotsModule.R")
sourceModule("scatterPlotsModule.R")
sourceModule("summaryTableModule.R")


# Define UI for application
ui <- fluidPage(
  titlePanel("PNA KNDy Data"),
  
  p("Be sure that the ",
    em("KNDyPNA_DATA.xlsx"), "file is up to date."),
  
  p("Make sure that the .Renviron file correctly points to this excel file."),
  
  p("To exclude cells, edit the ", 
    em("Exclusion"), "tab. Make the cell", 
    span("TRUE", style = "color:blue"), "to exclude."),
  
  tabsetPanel(
    ###Summary Plots Panel ----
    tabPanel(
      "Summary Plots",
      summaryPlotsUI("summaryPlots", KNDyDATA),
    ),
    ### Scatter Plots ----
    tabPanel(
      "Scatter Plots",
      scatterPlotsUI("scatterPlots", KNDyDATA)
    ),
    ### Summary tables ----
    tabPanel(
      "Summary Table + ANOVA",
      summaryTableUI("summaryTable", KNDyDATA)

    ),
    ### Raw Data ----
    tabPanel(
      "Raw Data",
      h2("Explore the Data"),
      
      #Show instructions
      checkboxInput(
        inputId = "instructions4",
        label = "Show instructions",
        value = FALSE
      ),
      
      #only show if show instructions is checked
      instructions4UI("instructions4Text"),
      
      #data table
      fluidRow(
        #column 1
        column(
          4,
          #select variables to include in DF
          varSelectInput(
            inputId = "vars_forTable",
            label = "Select variables for table",
            data = KNDyDATA,
            selected = c("CellID", "MouseID", "GenTreatment", "SpontAvgFiring"),
            multiple = TRUE)
        ),
        
        #column 2
        column(
          4,
          #which dataset?
          radioButtons(
            inputId = "dataset4", 
            label = "Which ages?",
            choices = list(
              "All",
              "Adults",
              "Juveniles"
            ),
            selected = "All"
          ),
          
          #Exclude
          checkboxInput(
            inputId = "exclude4",
            label = "Exclude marked cells",
            value = TRUE
          )
        ),
        
        #column 3
        column(
          4,
          #Which activity levels to include
          radioButtons(
            inputId = "firing4", 
            label = "Select level of activity:",
            choices = list(
              "All",
              "Quiescent",
              "Non-quiescent"
            ),
            selected = "All"
          )
        )
      ),
      #create a space for the data table output
      dataTableOutput("df")
    ),
    ### Maximum Burst Window ----
    tabPanel(
      "Maximum Burst Window",
      h3("Maximum Burst Window"),
      fluidRow(
        column(
          4,
          checkboxInput(
            "VBW_exclude",
            "Exclude marked cells?",
            value = TRUE
          )
        ),
        column(
          4,
          checkboxInput(
            "VBW_individualLines",
            "Plot individual cells?",
            value = FALSE
          )
        ),
        column(
          4,
          checkboxInput(
            "VBW_meanLines",
            "Plot mean lines?",
            value = TRUE
          )
        )
      ),
      plotOutput("VBW_plot"),
      dataTableOutput("VBW_peak_table")),
    
    #Cycles ----
    tabPanel(
      "Cycles",
      cyclesUI("Cycles")
    ),
    #Comparing Burst Window ----
    tabPanel(
      "Comp BW",
      compBurstWindowUI("compBW")
    ),
    #Firing Rate Graphs ----
    tabPanel(
      "Firing Rate",
      firingRateUI("firingRate")
    )
  ),
)


# Define server logic 
server <- function(input, output) {
  KNDy_VarNames <<-  KNDy_varNamesFunc(KNDyDATA)
  
  compBurstWindowServer(
    "compBW",
    KNDyDATA,
    KNDyDATA_adult = KNDyDATA_adult,
    KNDyDATA_juv = KNDyDATA_juv
  )
  
  ### SUMMARY PLOTS SERVER -------------------------------------
  summaryPlotsServer("summaryPlots", KNDyDATA, KNDyDATA_adult, KNDyDATA_juv, KNDy_VarNames)

  ### SCATTER PLOTS SERVER -------------------------------------
  scatterPlotsServer("scatterPlots", KNDyDATA, KNDyDATA_adult, KNDyDATA_juv, KNDy_VarNames)
  
  ### SUMMARY TABLE SERVER -------------------------------------
  summaryTableServer("summaryTable", KNDyDATA, KNDyDATA_adult, KNDyDATA_juv, KNDy_VarNames)
  
  
  ### PANEL 4 SERVER -------------------------------------
  output$df <- renderDataTable({
    data <- switch(
      input$dataset4,
      "All" = KNDyDATA,
      "Adults" = KNDyDATA_adult,
      "Juveniles" = KNDyDATA_juv
      
    )
    
    
    if(input$firing4 == "Quiescent"){
      data <- data %>%
        filter(Quiet == TRUE)
    }
    
    if(input$firing4 == "Non-quiescent"){
      data <- data %>%
        filter(Quiet == FALSE)
    }
    
    if(input$exclude4){
      data <- data %>%
        filter(Exclude == FALSE | is.na(Exclude)) #only include cells marked FALSE or NA for Exclude
    }
    
    data %>%
      select(!!! input$vars_forTable)
  })
  
  #Burst Window Server ---------------
  VBW_BurstsPerHour <- reactive({
    VBW_BurstsPerHour <- KNDyDATA %>%
      select(all_of(demoVarsAll_quo), all_of(timingVars_quo), MaxBurstWindow_spont, BurstsPerHour_0.01:BurstsPerHour_1.00)
    if(input$VBW_exclude){
      VBW_BurstsPerHour <- excludeFunc(VBW_BurstsPerHour) 
    }
    VBW_BurstsPerHour
  })
  
  VBW_BurstsPerHour_long <- reactive({
    VBW_BurstsPerHour_long <- make_long_form_burstsPerWindow(VBW_BurstsPerHour())
    VBW_BurstsPerHour_long <- VBW_BurstsPerHour_long %>%
      filter(!is.na(BurstsPerHour))
    
    VBW_BurstsPerHour_long <- make_BW_col(VBW_BurstsPerHour_long)
    VBW_BurstsPerHour_long
  })
  
  output$VBW_plot <- renderPlot({
    VBW_plot_lines(
      VBW_BurstsPerHour_long(), 
      individualLines = input$VBW_individualLines,
      mean_lines = input$VBW_meanLines
    )
  })
  
  output$VBW_peak_table <- renderDataTable({
    VBW_BurstsPerHour_grouped <- getAvgByTreatAge(
      VBW_BurstsPerHour() %>% 
        select(-Sac_hr, - Record_start_hr, -Record_end_hr))
    VBW_BurstsPerHour_grouped
    
    VBW_BurstsPerHour_grouped_long <- make_long_form_burstsPerWindow(VBW_BurstsPerHour_grouped)
    VBW_BurstsPerHour_grouped_long <- VBW_BurstsPerHour_grouped_long %>%
      filter(!is.na(BurstsPerHour))
    VBW_BurstsPerHour_grouped_long
    
    VBW_BurstsPerHour_grouped_long <- make_BW_col(VBW_BurstsPerHour_grouped_long)
    VBW_BurstsPerHour_grouped_long
    
    #Control juvenile data frame from the grouped, long-form data
    VBW_ConJuv <- VBW_BurstsPerHour_grouped_long %>%
      filter(GenTreatment == "Control" & AgeGroup == "Juvenile")
    
    #Find the row where the maximum bursts per hour occurs for this df
    VBW_ConJuv_peak <- VBW_ConJuv[which(VBW_ConJuv$BurstsPerHour == max(VBW_ConJuv$BurstsPerHour, na.rm = TRUE)),]
    
    #Control adult data frame from the grouped, long-form data
    VBW_ConAdult <- VBW_BurstsPerHour_grouped_long %>%
      filter(GenTreatment == "Control" & AgeGroup == "Adult")
    
    #Find the row where the maximum bursts per hour occurs for this df
    VBW_ConAdult_peak <- VBW_ConAdult[which(VBW_ConAdult$BurstsPerHour == max(VBW_ConAdult$BurstsPerHour, na.rm = TRUE)),]
    
    #PNA Juvenile data frame from the grouped, long-form data
    VBW_PNAJuv <- VBW_BurstsPerHour_grouped_long %>%
      filter(GenTreatment == "PNA" & AgeGroup == "Juvenile")
    
    #Find the row where the maximum bursts per hour occurs for this df
    VBW_PNAJuv_peak <- VBW_PNAJuv[which(VBW_PNAJuv$BurstsPerHour == max(VBW_PNAJuv$BurstsPerHour, na.rm = TRUE)),]
    
    #PNA adult data frame from the grouped, long-form data
    VBW_PNAAdult <- VBW_BurstsPerHour_grouped_long %>%
      filter(GenTreatment == "PNA" & AgeGroup == "Adult")
    
    #Find the row where the maximum bursts per hour occurs for this df
    VBW_PNAAdult_peak <- VBW_PNAAdult[which(VBW_PNAAdult$BurstsPerHour == max(VBW_PNAAdult$BurstsPerHour, na.rm = TRUE)),]
    
    #Pull into one dataframe
    VBW_group_peaks <- rbind.data.frame(VBW_ConJuv_peak, VBW_ConAdult_peak, VBW_PNAJuv_peak, VBW_PNAAdult_peak)
    VBW_group_peaks
  })
  
  #Cycles Server ---------------
  cyclesServer("Cycles", KNDy_cycles = KNDy_cycles)
  
  #Firing Rate Server -----------
  firingRateServer("firingRate", KNDy_firingRate = KNDy_firingRate2)
}
# Run the application 
shinyApp(ui = ui, server = server)
