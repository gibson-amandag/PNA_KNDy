### Raw Data Module

# https://shiny.rstudio.com/articles/modules.html

rawDataUI <- function(
  id,
  KNDyDATA
){
  ns <- NS(id)
  tagList(
    h2("Explore the Data"),
    
    #only show if show instructions is checked
    instructionsRawDataUI(ns("instructionsText")),
    
    #data table
    fluidRow(
      #column 1
      column(
        4,
        #select variables to include in DF
        varSelectInput(
          inputId = ns("vars_forTable"),
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
          inputId = ns("dataset4"), 
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
          inputId = ns("exclude4"),
          label = "Exclude marked cells",
          value = TRUE
        )
      ),
      
      #column 3
      column(
        4,
        #Which activity levels to include
        radioButtons(
          inputId = ns("firing4"), 
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
    dataTableOutput(ns("df"))
    
  )
}


rawDataServer <- function(
  id,
  KNDyDATA,
  KNDyDATA_adult,
  KNDyDATA_juv
){
  moduleServer(
    id,
    function(input, output, session) {
      instructionsRawDataServer("instructionsText")
      
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
    }
  )
}

