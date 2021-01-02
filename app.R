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
#panel 1 instructions
sourceModule("panel1_instructions.R")
#panel 2 instructions
sourceModule("panel2_instructions.R")
#panel 3 instructions
sourceModule("panel3_instructions.R")
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
      # h2("Create summary plots for variables"),
      # 
      # #Show instructions
      # checkboxInput(
      #   inputId = "instructions",
      #   label = "Show instructions",
      #   value = FALSE
      # ),
      # 
      # instructions1UI("instructionsText"),
      # 
      # #create a row for input selections
      # fluidRow(
      #   #column 1
      #   column(
      #     3, 
      #     #variable to plot
      #     varSelectInput(
      #       inputId = "var_to_plot",
      #       label = "Select variable to plot",
      #       data = KNDyDATA %>%
      #         select(SpontAvgFiring:MaxBurstWindow_senktide)
      #     ),
      #     #treatment grouping
      #     varSelectInput(
      #       inputId = "treatment",
      #       label = "Select treatment type",
      #       data = KNDyDATA %>%
      #         select(Treatment, GenTreatment),
      #       selected = "GenTreatment"
      #     ),
      #     #grouping variable (on x axis)
      #     
      #     #grouping variable separated out so that it depends on which datasets.
      #     #If there's only a single group on the x-axis, get a JSON error
      #     
      #     #if juveniles and all firing
      #     conditionalPanel(
      #       condition = "input.dataset == 'Juveniles' & input.firing == 'All'",
      #       varSelectInput(
      #         inputId = "group_juv_All",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             Who,
      #             Sac_9plus, Quiet
      #           ),
      #         selected = "Who"
      #       )
      #     ),
      #     
      #     #if juveniles and not all firing
      #     conditionalPanel(
      #       condition = "input.dataset == 'Juveniles' & input.firing != 'All'",
      #       varSelectInput(
      #         inputId = "group_juv_notAll",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             Who,
      #             Sac_9plus
      #           ),
      #         selected = "Who"
      #       )
      #     ),
      #     
      #     #if adults and all firing
      #     conditionalPanel(
      #       condition = "input.dataset == 'Adults' & input.firing == 'All'",
      #       varSelectInput(
      #         inputId = "group_adult_All",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             Sac_9plus,
      #             Quiet
      #           ),
      #         selected = "GenTreatment"
      #       )
      #     ),
      #     
      #     #if adults and not all firing
      #     conditionalPanel(
      #       condition = "input.dataset == 'Adults' & input.firing != 'All'",
      #       varSelectInput(
      #         inputId = "group_adult_notAll",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             Sac_9plus
      #           ),
      #         selected = "GenTreatment"
      #       )
      #     ),
      #     
      #     #if all for both
      #     conditionalPanel(
      #       condition = "input.dataset == 'All' & input.firing == 'All'",
      #       varSelectInput(
      #         inputId = "group_All_All",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             AgeGroup,
      #             Who,
      #             Sac_9plus, Quiet
      #           ),
      #         selected = "AgeGroup"
      #       )
      #     ),
      #     
      #     #if all for data set, not all for firing
      #     conditionalPanel(
      #       condition = "input.dataset == 'All' & input.firing != 'All'",
      #       varSelectInput(
      #         inputId = "group_All_notAll",
      #         label = "Select grouping variable",
      #         data = KNDyDATA %>%
      #           select(
      #             Treatment,
      #             GenTreatment,
      #             AgeGroup,
      #             Who,
      #             Sac_9plus),
      #         selected = "AgeGroup"
      #       )
      #     )
      #     
      #   ),
      #   
      #   #column 2
      #   column(
      #     3,
      #     #expand to zero
      #     checkboxInput(
      #       inputId = "expand_to_zero",
      #       "Expand to zero",
      #       value = TRUE
      #     ),
      #     #zoom y axis
      #     checkboxInput(
      #       inputId = "zoom",
      #       label = "Zoom to limit y axis",
      #       value = FALSE
      #     ),
      #     #Zoom Range
      #     #only show if zoom is true https://shiny.rstudio.com/articles/dynamic-ui.html
      #     conditionalPanel(
      #       condition = "input.zoom == true",
      #       numericInput(
      #         inputId = "ylim",
      #         label = "Enter upper y limit:",
      #         value = 20
      #       )
      #     ),
      #     #dot plot layer
      #     conditionalPanel(
      #       condition = "input.zoom == false",
      #       checkboxInput(
      #         inputId = "dot_plot",
      #         label = "Add dot plot layer",
      #         value = TRUE
      #       )
      #     ),
      #     
      #     #violin plot layer
      #     # conditionalPanel(
      #     #   condition = "input.zoom == false",
      #     #   checkboxInput(
      #     #     inputId = "violin_plot",
      #     #     label = "Add violin plot layer",
      #     #     value = TRUE
      #     #   )
      #     # )
      #     
      #     checkboxInput(
      #       inputId = "violin_plot",
      #       label = "Add violin plot layer",
      #       value = TRUE
      #     ),
      #   ),
      #   
      #   #column 3
      #   column(
      #     3,
      #     #dot size
      #     sliderInput(
      #       inputId = "dotsize",
      #       label = "Enter dot size:",
      #       step = 0.1,
      #       min = 0,
      #       max = 10,
      #       value = 6
      #     ),
      #     #bin width
      #     numericInput(
      #       inputId = "binwidth",
      #       label = "Enter binwidth:",
      #       value = 0.05
      #     ),
      #     #position width
      #     sliderInput(
      #       inputId = "positionWidth",
      #       label = "Enter position width:",
      #       min = 0,
      #       max = 1,
      #       value = 0.9
      #     )
      #   ),
      #   
      #   #column 4
      #   column(
      #     3,
      #     #Exclude
      #     checkboxInput(
      #       inputId = "exclude",
      #       label = "Exclude marked cells",
      #       value = TRUE
      #     ),
      #     
      #     #which dataset?
      #     radioButtons(
      #       inputId = "dataset", 
      #       label = "Which ages?",
      #       choices = list(
      #         "All",
      #         "Adults",
      #         "Juveniles"
      #       ),
      #       selected = "All"
      #     ),
      #     #Which activity levels to include
      #     radioButtons(
      #       inputId = "firing", 
      #       label = "Select level of activity:",
      #       choices = list(
      #         "All",
      #         "Quiescent",
      #         "Non-quiescent"
      #       ),
      #       selected = "All"
      #     )
      #   )
      #   
      # ),
      # 
      # #Create a space for the output plot to be placed
      # plotOutput(
      #   "plot",
      #   click = "plot_click"
      # ),
      # # https://shiny.rstudio.com/articles/plot-interaction.html - interactive plot info. Using click
      # 
      # h4("Values within binwidth of click"),
      # 
      # #Text output
      # verbatimTextOutput("value_info"),
      # 
      # #DataFrame based on click y-value
      # dataTableOutput("plot_info")
    ),
    ### Scatter Plots ----
    tabPanel(
      "Scatter Plots",
      h2("Create scatter plots between variables"),
      #Show instructions
      checkboxInput(
        inputId = "instructions2",
        label = "Show instructions",
        value = FALSE
      ),
      
      # #only show if show instructions is checked
      instructions2UI("instructions2Text"),
      
      
      #Create a row for input selections, specifically for variables
      fluidRow(
        column(
          4, 
          #y axis variable - var_to_plot
          varSelectInput(
            inputId = "yaxis",
            label = "Select variable to plot on y axis",
            data = KNDyDATA %>%
              select(SpontAvgFiring:MaxBurstWindow_senktide),
            selected = "MaxBurstWindow_spont"
          )
        ),
        column(
          4,
          #plot by variable
          varSelectInput(
            inputId = "xaxis",
            label = "Select variable to plot by",
            data = KNDyDATA %>%
              select(
                SpontAvgFiring,
                SpontLength_min,
                Record_start_hr,
                Record_end_hr,
                Sac_hr,
                Recording_date,
                UterineMass,
                Age_in_days,
                MaxBurstWindow_spont
              )
          )
        ),
        column(
          4,
          #treatment grouping
          varSelectInput(
            inputId = "treatment2",
            label = "Select treatment type",
            data = KNDyDATA %>%
              select(Treatment, GenTreatment),
            selected = "GenTreatment"
          )
        )
      ),
      
      #sidebar for additional options about data sets
      sidebarLayout(
        sidebarPanel(
          #Exclude
          checkboxInput(
            inputId = "exclude2",
            label = "Exclude marked cells",
            value = TRUE
          ),
          
          #which dataset?
          radioButtons(
            inputId = "dataset2",
            label = "Which ages?",
            choices = list(
              "All",
              "Adults",
              "Juveniles"
            ),
            selected = "All"
          ),
          #Which activity levels to include
          radioButtons(
            inputId = "firing2",
            label = "Select level of activity:",
            choices = list(
              "All",
              "Quiescent",
              "Non-quiescent"
            ),
            selected = "All"
          )
        ),
        mainPanel(
          #Create a space for the scatter plot output
          plotOutput(
            "scatterPlot",
            click = "scatterPlot_click"
          ),
          
          h4("Points Near Click"),
          
          #space to print table with clicked on data point
          verbatimTextOutput("scatter_info"),
        ),
      )),
    ### Summary tables ----
    tabPanel(
      "Summary Table + ANOVA",
      h2("Generate summary data for variables"),
      
      #Show instructions
      checkboxInput(
        inputId = "instructions3",
        label = "Show instructions",
        value = FALSE
      ),
      
      #only show if show instructions is checked
      instructions3UI("instructions3Text"),
      
      #Summary information
      fluidRow(
        #column 1
        column(
          3,
          #grouping variables
          varSelectInput(
            inputId = "group_vars",
            label = "Select variables to group by",
            data = KNDyDATA %>%
              select(CellID:Who, Sac_9plus, Quiet),
            selected = c("GenTreatment", "AgeGroup"),
            multiple = TRUE
          )
        ),
        
        #column 2
        column(
          3,
          #variable to summarize
          varSelectInput(
            inputId = "var_toSummarize",
            label = "Select variable to summarize",
            data = KNDyDATA %>%
              select(SpontAvgFiring:MaxBurstWindow_senktide),
            selected = "SpontAvgFiring"
          )
        ),
        
        #column 3
        column(
          3,
          #which dataset?
          radioButtons(
            inputId = "dataset3", 
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
            inputId = "exclude3",
            label = "Exclude marked cells",
            value = TRUE
          )
        ),
        
        #column 4
        column(
          3,
          #Which activity levels to include
          radioButtons(
            inputId = "firing3", 
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
      dataTableOutput("summarydf"),
      
      ### ANOVAs ----
      h2("ANOVAs"),
      
      varSelectInput(
        "ANOVA_var",
        "Select variable for ANOVA:",
        data = KNDyDATA %>%
          select(SpontAvgFiring:MaxBurstWindow_senktide),
        selected = "SpontAvgFiring"
      ),
      
      htmlOutput("ANOVA_table")
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
  
  # # https://groups.google.com/g/shiny-discuss/c/7ZO92Oy30Dw (possibility to add horizontal
  # #line when click. But right now this disappears on me. Even with fix suggested here)
  # 
  # # observeEvent(input$plot_click,
  # #              y_click <- input$plot_click$y)
  # 
  # output$plot <- renderPlot({
  #   data1 <- switch(
  #     input$dataset,
  #     "All" = KNDyDATA,
  #     "Adults" = KNDyDATA_adult,
  #     "Juveniles" = KNDyDATA_juv
  #     
  #   )
  #   
  #   if(input$firing == "Quiescent"){
  #     data1 <- data1 %>%
  #       filter(Quiet == TRUE)
  #   }
  #   
  #   if(input$firing == "Non-quiescent"){
  #     data1 <- data1 %>%
  #       filter(Quiet == FALSE)
  #   }
  #   
  #   if(input$exclude){
  #     data1 <- data1 %>%
  #       filter(Exclude == FALSE | is.na(Exclude)) #only include cells marked FALSE or NA for Exclude
  #   }
  #   
  #   #grouping variable depending on what datasets
  #   if(input$dataset == "All" & input$firing == "All"){
  #     grouping_var <- input$group_All_All
  #   }else if(input$dataset == "All" & input$firing != "All"){
  #     grouping_var <- input$group_All_notAll
  #   }else if(input$dataset == "Juveniles" & input$firing == "All"){
  #     grouping_var <- input$group_juv_All
  #   }else if(input$dataset == "Juveniles" & input$firing != "All"){
  #     grouping_var <- input$group_juv_notAll
  #   }else if(input$dataset == "Adults" & input$firing == "All"){
  #     grouping_var <- input$group_adult_All
  #   }else if(input$dataset == "Adults" & input$firing != "All"){
  #     grouping_var <- input$group_adult_notAll
  #   }
  #   
  #   data1 <<- data1
  #   
  #   #https://github.com/rstudio/shiny/issues/2673 getting error warning without print when x is a single value
  #   #but printing doesn't let interact. If want to interact (for example, show CellID when hover, needs to not be printed)
  #   #https://shiny.rstudio.com/reference/shiny/1.3.1/plotOutput.html
  #   data1 %>%
  #     filter(!is.na(!! input$var_to_plot)) %>%
  #     ggplot(aes(x = !! grouping_var, y = !! input$var_to_plot, fill = !! input$treatment))+
  #     my_KNDy_geoms(
  #       dotsize = input$dotsize,
  #       binwidth = input$binwidth,
  #       positionWidth = input$positionWidth,
  #       xtitle = NULL,
  #       ytitle = NULL,
  #       var_to_plot = input$var_to_plot,
  #       title = NULL,
  #       expand_to_zero = input$expand_to_zero,
  #       dot_plot = ifelse(input$zoom, TRUE, input$dot_plot),
  #       violin_plot = input$violin_plot,
  #       zoom_y = input$zoom,
  #         #ifelse(is.na(input$zoom), FALSE, input$zoom),
  #         # input$zoom,
  #       ylimit = 20,
  #         #input$ylim,
  #       mean_plot = TRUE
  #     )
  #   # if(!is.null(input$plot_click$y))
  #   #   geom_hline(yintercept = y_click)
  #   
  #   
  # })
  # 
  # output$value_info <- renderText(
  #   if(is.null(input$plot_click$y)){
  #     paste("Click on graph")
  #   }else({
  #     paste0(
  #       "Looking for values between ", #one binwidth +/- click y value
  #       round(input$plot_click$y - input$binwidth, 3),
  #       " and ",
  #       round(input$plot_click$y + input$binwidth, 3)
  #     )
  #   })
  # )
  # 
  # 
  # output$plot_info <- renderDataTable(
  #   if(!is.null(input$plot_click$y)){
  #     data1 %>%
  #       select(
  #         CellID,
  #         MouseID,
  #         #if the variable to plot is not Spontaneous Firing Rate, also display this column
  #         if(input$var_to_plot != sym("SpontAvgFiring")){sym("SpontAvgFiring")},
  #         !! input$var_to_plot,
  #         Treatment,
  #         AgeGroup,
  #         Who
  #       ) %>%
  #       filter(
  #         #look for values that are near the click y-value. The tolerance is the binwidth
  #         near(!! input$var_to_plot, input$plot_click$y, tol = input$binwidth)
  #       )
  #   }
  # )
  
  
  ### PANEL 2 SERVER -------------------------------------
  output$scatterPlot = renderPlot({
    data2 <-  switch(
      input$dataset2,
      "All" = KNDyDATA,
      "Adults" = KNDyDATA_adult,
      "Juveniles" = KNDyDATA_juv
      
    )
    
    if(input$firing2 == "Quiescent"){
      data2 <- data2 %>%
        filter(Quiet == TRUE)
    }
    
    if(input$firing2 == "Non-quiescent"){
      data2 <- data2 %>%
        filter(Quiet == FALSE)
    }
    
    if(input$exclude2){
      data2 <- data2 %>%
        filter(Exclude == FALSE | is.na(Exclude)) #only include cells marked FALSE or NA for Exclude
    }
    
    data2 <<- data2
    
    data2 %>%
      filter(!is.na(!! input$yaxis)) %>%
      ggplot(aes(x = !! input$xaxis, y = !! input$yaxis, colour = !! input$treatment2))+
      geom_point(size = 3)+
      geom_smooth(method = lm, se = FALSE, formula = y ~ x)+
      labs(
        x = KNDy_VarNames[, as.character(input$xaxis)], 
        y = KNDy_VarNames[, as.character(input$yaxis)], 
        title = KNDy_VarNames[, as.character(input$yaxis)], 
        subtitle = "By Firing Rate", 
        fill = "Treatment"
      )+
      my_theme
    
  })
  
  
  output$scatter_info <- renderPrint(
    
    if(is.null(input$scatterPlot_click)){
      "Click on a point to display values"
    }else(
      nearPoints(
        data2 %>%
          select(
            CellID,
            MouseID,
            if(input$xaxis != sym("SpontAvgFiring") | 
               input$yaxis != sym("SpontAvgFiring"))
            {sym("SpontAvgFiring")},
            !! input$xaxis,
            !! input$yaxis,
            Treatment
          ),
        input$scatterPlot_click
      )
    )
  )
  
  
  ### PANEL 3 SERVER -------------------------------------
  output$summarydf <- renderDataTable({
    data <- switch(
      input$dataset3,
      "All" = KNDyDATA,
      "Adults" = KNDyDATA_adult,
      "Juveniles" = KNDyDATA_juv
      
    )
    
    if(input$firing3 == "Quiescent"){
      data <- data %>%
        filter(Quiet == TRUE)
    }
    
    if(input$firing3 == "Non-quiescent"){
      data <- data %>%
        filter(Quiet == FALSE)
    }
    
    if(input$exclude3){
      data <- data %>%
        filter(Exclude == FALSE | is.na(Exclude)) #only include cells marked FALSE or NA for Exclude
    }
    
    data %>%
      filter(!is.na(!! input$var_toSummarize))%>%
      group_by(!!! input$group_vars) %>% #group by the grouping variables
      summarize(
        Mean = mean(!! input$var_toSummarize, na.rm = TRUE),
        SD = sd(!! input$var_toSummarize, na.rm = TRUE),
        n = n(),
        SEM = SD/sqrt(n),
        .groups = 'drop'
      )
    
    
  })
  
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
  
  ### ANOVA SERVER -----------------
  output$ANOVA_table <- renderText({
    KNDyDATA_contrasts <- makeTreatandAgeContrasts(KNDyDATA)
    
    Model <- lm_byTreatxAge(input$ANOVA_var, KNDyDATA_contrasts)
    
    ANOVA_table <- makeANOVA_TreatAge(Model)
    ANOVA_table %>%
      kable_styling(
        font_size = 18,
        bootstrap_options = c("striped"), full_width = TRUE
      )
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
