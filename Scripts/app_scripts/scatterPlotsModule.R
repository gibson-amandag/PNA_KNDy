### Scatter Plots Module

# https://shiny.rstudio.com/articles/modules.html

scatterPlotsUI <- function(
  id
){
  ns <- NS(id)
  tagList(
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
      )
    )
  )
}


scatterPlotsServer <- function(
  id
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )
}

