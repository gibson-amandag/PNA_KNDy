### Firing Rate Charts

# https://shiny.rstudio.com/articles/modules.html

firingRateUI <- function(id
                     ){
  ns <- NS(id)
  tagList(
    
    h3("Firing Rate"),
    p("This will take a LONG time to load. Just be patient"),
    p("Changing the limits for the axes will change it for all graphs"),
    
    zoomAxisUI(ns("zoom_x"), "x"),
    zoomAxisUI(ns("zoom_y"), "y"),
    
    tabsetPanel(
      tabPanel("Con-Adults",
                h4("Control - Adults"),
               plotOutput(ns("Con_Adults"), height = "1500px")
               ),
      tabPanel("PNA-Adults",
               h4("PNA - Adults"),
               plotOutput(ns("PNA_Adults"), height = "1500px")
               ),
      tabPanel("Con-Juv",
               h4("Control - Juveniles"),
               plotOutput(ns("Con_Juveniles"), height = "1200px")
               ),
      tabPanel("PNA-Juv",
               h4("PNA - Juveniles"),
               plotOutput(ns("PNA_Juveniles"), height = "1200px")
               ),
      tabPanel("Individual Cell",
               h4("Individual Cell"),
               p("This hasn't been written yet"),
               
               #Need inputs to select which cell
               plotOutput(ns("individualCell"))
               )
      
    )

    )
}


firingRateServer <- function(id,
                         KNDy_firingRate
){
  moduleServer(
    id,
    function(input, output, session) {
      #Zoom X
      zoom_x <- zoomAxisServer("zoom_x", "x", 0, 165)
      
      #Zoom Y
      zoom_y <- zoomAxisServer("zoom_y", "y", 0, 20)
      
      #Add filter for exclusion
      
      KNDy_firingRate_long <- reactive({
        KNDy_firingRate_long <- KNDy_firingRate %>%
          excludeFunc() %>%
          # filter(Exclude == FALSE | is.na(Exclude)) %>% #remove excluded cells
          make_firing_long()
        
        KNDy_firingRate_long <- KNDy_firingRate_long %>%
          add_Min_col()
        
        KNDy_firingRate_long <- KNDy_firingRate_long %>%
          excludeFunc()
        
        return(KNDy_firingRate_long)
      })
      #Add ylimit filters
      output$Con_Adults <- renderPlot({
        KNDy_firingRate_long() %>%
          filter(TreatxAge == "Con-Adult") %>%
          firingRatePlotFunc(zoom_x = zoom_x$zoom(),
                             xmin = zoom_x$min(),
                             xmax = zoom_x$max(),
                             zoom_y = zoom_y$zoom(),
                             ymin = zoom_y$min(),
                             ymax = zoom_y$max())
      })
      output$PNA_Adults <- renderPlot({
        KNDy_firingRate_long() %>%
          filter(TreatxAge == "PNA-Adult") %>%
          firingRatePlotFunc(zoom_x = zoom_x$zoom(),
                             xmin = zoom_x$min(),
                             xmax = zoom_x$max(),
                             zoom_y = zoom_y$zoom(),
                             ymin = zoom_y$min(),
                             ymax = zoom_y$max())
      })
      output$Con_Juveniles <- renderPlot({
        KNDy_firingRate_long() %>%
          filter(TreatxAge == "Con-Juvenile") %>%
          firingRatePlotFunc(zoom_x = zoom_x$zoom(),
                             xmin = zoom_x$min(),
                             xmax = zoom_x$max(),
                             zoom_y = zoom_y$zoom(),
                             ymin = zoom_y$min(),
                             ymax = zoom_y$max())
      })
      output$PNA_Juveniles <- renderPlot({
        KNDy_firingRate_long() %>%
          filter(TreatxAge == "PNA-Juvenile") %>%
          firingRatePlotFunc(zoom_x = zoom_x$zoom(),
                             xmin = zoom_x$min(),
                             xmax = zoom_x$max(),
                             zoom_y = zoom_y$zoom(),
                             ymin = zoom_y$min(),
                             ymax = zoom_y$max())
      })
      
    }
  )
}

