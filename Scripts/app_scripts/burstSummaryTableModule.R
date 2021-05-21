### Summary Table Module

# https://shiny.rstudio.com/articles/modules.html

burstSummaryTableUI <- function(
  id,
  bParamsDF
){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        fileInput(
          ns("bParamsFile"),
          label = "Select Burst Params File",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx")
        )
      ),
      column(
        6, 
        p("If selecting an excel file..."),
        textInput(
          ns("sheetName"),
          label = "Enter Sheet Name",
          value = "Sheet1"
        )
      )
    ),
    
    #Summary information
    fluidRow(
      #column 1
      column(
        6,
        #grouping variables
        varSelectInput(
          inputId = ns("group_vars"),
          label = "Select variables to group by",
          data = bParamsDF %>%
            select(
              MouseNum,
              Treatment,
              GenTreatment,
              CellNum,
              AgeGroup,
              Who,
              WhoRecorded
              ),
          selected = c("AgeGroup", "GenTreatment"),
          multiple = TRUE
        )
      ),
      
      #column 2
      column(
        6,
        #variable to summarize
        varSelectInput(
          inputId = ns("var_toSummarize"),
          label = "Select variable to summarize",
          data = bParamsDF %>%
            select(bn:ssFlags),
          selected = "bf"
        )
      )
    ),
    tabsetPanel(
      tabPanel(
        "Mean Summaries",
        h2("Summary data for variable"),
        #create a space for the data table output
        shiny::dataTableOutput(ns("summarydf"))
      ),
      tabPanel(
        "Quartile Summaries",
        h2("Quartile Summaries"),
        shiny::dataTableOutput(ns("quartiledf"))
      ),
      tabPanel(
        "ANOVA",
        h2("ANOVA"),
        htmlOutput(ns("ANOVA_table"))
      ),
      tabPanel(
        "Plot",
        h2("Plot Data"),
        fluidRow(
          column(
            4,
            #expand to zero
            checkboxInput(
              inputId = ns("expand_to_zero"),
              "Expand to zero",
              value = TRUE
            ),
            #zoom y axis
            # checkboxInput(
            #   inputId = ns("zoom"),
            #   label = "Zoom to limit y axis",
            #   value = FALSE
            # ),
            
            uiOutput(ns("dotPlotUI")),
            
            checkboxInput(
              inputId = ns("violin_plot"),
              label = "Add violin plot layer",
              value = TRUE
            )
          ),
          
          #column 2
          column(
            4,
            #dot size
            sliderInput(
              inputId = ns("dotsize"),
              label = "Enter dot size:",
              step = 0.1,
              min = 0,
              max = 10,
              value = 1
            ),
            #binwidth
            
            #position width
            sliderInput(
              inputId = ns("positionWidth"),
              label = "Enter position width:",
              min = 0,
              max = 1,
              value = 0.9
            )
          ),
          column(
            4,
            checkboxInput(
              ns("selectBinWidth"),
              "Manually Adjust Bin Width",
              FALSE
            ),
            uiOutput(ns("binwidthUI"))
          )
        ),
        
        zoomAxisUI(ns("zoom_y"), "y"),
        
        plotOutput(
          ns("burstPlot"),
         click = ns("plot_click")
        ),
        h4("Values within binwidth of click"),
        
        #Text output
        verbatimTextOutput(ns("value_info")),
        
        #DataFrame based on click y-value
        shiny::dataTableOutput(ns("plot_info")),
      ),
      tabPanel(
        "Data Table - filtering",
        h3("Data Table"),
        
        p("If you select a row in this data table, it will exclude it from the calculations above"),
        DTOutput(ns("burstTable"))
      )
    )
  )
}


burstSummaryTableServer <- function(
  id,
  KNDyDATA,
  KNDy_VarNames #change to bParams var names
){
  moduleServer(
    id,
    function(input, output, session) {
      bParamsDF = reactive({
        inFile <- input$bParamsFile
        
        if(is.null(inFile))
          return(NULL)
        
        extension = file_ext(inFile$datapath)
        
        if(extension == "xlsx") {
          bParamsDF <- myXLSX_funcFileSelect(inFile$datapath, input$sheetName)
        } else {
          bParamsDF <- read.csv(inFile$datapath, sep = "\t")
          bParamsDF <- as.data.frame(bParamsDF)
        }
        
        bParamsDFList <- getBurstParamsDF(KNDyDATA, bParamsDF)
        bParamsDF <- bParamsDFList$bParamsDF
        
        bParamsDF
      })
      # filterOutput <- filterDFServer("filterDF", bParamsDF())
      
      output$summarydf <- shiny::renderDataTable({
        # data <- filterOutput$filteredDF()
        data <- bParamsDF()
        
        #filter out selected rows from the data table
        if(length(input$burstTable_rows_selected)){
          data <- data %>%
            filter(
              ! (row_number() %in% input$burstTable_rows_selected)
            )
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
      
      quibble <- function(x, q = c(0.25, 0.5, 0.75)) {
        tibble(x = quantile(x, q), q = q)
      }
      
      output$quartiledf <- shiny::renderDataTable({
        # data <- filterOutput$filteredDF()
        data <- bParamsDF()
        
        #filter out selected rows from the data table
        if(length(input$burstTable_rows_selected)){
          data <- data %>%
            filter(
              ! (row_number() %in% input$burstTable_rows_selected)
            )
        }
        
        data %>%
          filter(!is.na(!! input$var_toSummarize))%>%
          group_by(!!! input$group_vars) %>% #group by the grouping variables
          # summarize(
          #   quibble(!! input$var_toSummarize),
          #   .groups = 'drop'
          # )
          summarize(
            min = min(!! input$var_toSummarize, na.rm = TRUE),
            q1 = quantile(!! input$var_toSummarize, 0.25, na.rm = TRUE),
            median = median(!! input$var_toSummarize, na.rm=TRUE),
            q3 = quantile(!! input$var_toSummarize, 0.75, na.rm=TRUE),
            max = max(!! input$var_toSummarize, na.rm = TRUE),
            .groups = 'drop'
          )
      })
      
      output$ANOVA_table <- renderText({
        # data <- filterOutput$filteredDF()
        data <- bParamsDF()
        
        #filter out selected rows from the data table
        if(length(input$burstTable_rows_selected)){
          data <- data %>%
            filter(
              ! (row_number() %in% input$burstTable_rows_selected)
            )
        }
        
        # The makeTreatandAgeContrasts function excludes cells
        bParamsDF_contrasts <- makeTreatandAgeContrasts(data)
        
        Model <- lm_byTreatxAge(input$var_toSummarize, bParamsDF_contrasts)
        
        ANOVA_table <- makeANOVA_TreatAge(Model)
        ANOVA_table %>%
          kable_styling(
            font_size = 18,
            bootstrap_options = c("striped"), full_width = TRUE
          )
      })
      
      zoom_y <- zoomAxisServer("zoom_y", "y", 0, 20)
      
      #ylimit UI to change with checking of zoom input
      output$ylimUI <- renderUI({
        validate(
          need(zoom_y$zoom() == TRUE, FALSE)
        )
        ns <- session$ns
        numericInput(
          inputId = ns("ylim"),
          label = "Enter upper y limit:",
          value = 20
        )
      })
      
      output$dotPlotUI <- renderUI({
        validate(
          need(zoom_y$zoom() == FALSE, FALSE)
        )
        ns <- session$ns
        checkboxInput(
          inputId = ns("dot_plot"),
          label = "Add dot plot layer",
          value = TRUE
        )
      })
      
      output$binwidthUI <- renderUI({
        validate(
          need(input$selectBinWidth == TRUE, FALSE)
        )
        ns <- session$ns
        numericInput(
          inputId = ns("binwidth"),
          label = "Enter binwidth:",
          value = 0.05
        )
      })
      
      output$burstPlot <- renderPlot({
        data <- bParamsDF()
        
        #filter out selected rows from the data table
        if(length(input$burstTable_rows_selected)){
          data <- data %>%
            filter(
              ! (row_number() %in% input$burstTable_rows_selected)
            )
        }
        
        if(input$selectBinWidth == TRUE){
          binWidth <- input$binwidth
        } else{
          binWidth <- NULL
        }
        
        data %>%
          filter(!is.na(!! input$var_toSummarize)) %>%
          ggplot(aes(x = AgeGroup, y = !! input$var_toSummarize, fill = GenTreatment))+
          my_KNDy_geoms(
            dotsize = input$dotsize,
            binwidth = binWidth,
            positionWidth = input$positionWidth,
            xtitle = NULL,
            ytitle = NULL,
            var_to_plot = input$var_toSummarize,
            title = NULL,
            expand_to_zero = input$expand_to_zero,
            #Because dot plot is rendered, graph loads once while it's still null
            dot_plot = ifelse(zoom_y$zoom(), TRUE, input$dot_plot),
            violin_plot = input$violin_plot,
            zoom_y = zoom_y$zoom(),
            # if the max value is null, use 20 (this shouldn't last long,
            # but graph loads once before the values for max/min have loaded when the checkbox is clicked
            ylimit = ifelse(!is.null(zoom_y$max()), zoom_y$max(), 20),
            mean_plot = TRUE,
            # if the min value is null, use 20
            ymin = ifelse(!is.null(zoom_y$min()), zoom_y$min(), 0)
          )
      })
      
      output$burstTable <- renderDT({
        bParamsDF() %>%
          select(
            CellID:ssFlags,
            AgeGroup,
            Treatment,
            GenTreatment,
            Who,
            WhoRecorded
          )
      })
      
      thisBinWidth <- reactive({
        if(input$selectBinWidth == TRUE){
          binWidth <- input$binwidth
          
        } else{
          sumDF <- bParamsDF() %>%
            select(
              !! input$var_toSummarize
            )
          minVal = min(sumDF, na.rm = TRUE)
          maxVal = max(sumDF, na.rm = TRUE)
          
          binWidth <- (maxVal - minVal) / 30
        }
        return (binWidth)
      })
      
      output$value_info <- renderText(
        if(is.null(input$plot_click$y)){
          paste("Click on graph")
        }else({
          paste0(
            "Looking for values between ", #one binwidth +/- click y value
            round(input$plot_click$y - thisBinWidth(), 3),
            " and ",
            round(input$plot_click$y + thisBinWidth(), 3)
          )
        })
      )
      
      
      output$plot_info <- shiny::renderDataTable(
        if(!is.null(input$plot_click$y)){
          bParamsDF() %>%
            select(
              CellID,
              MouseID,
              #if the variable to plot is not Spontaneous Firing Rate, also display this column
              if(input$var_toSummarize != sym("tf")){sym("tf")},
              !! input$var_toSummarize,
              GenTreatment,
              AgeGroup,
              Who,
              WhoRecorded
            ) %>%
            filter(
              #look for values that are near the click y-value. The tolerance is the binwidth
              near(!! input$var_toSummarize, input$plot_click$y, tol = thisBinWidth())
            )
        }
      )
      
    }
  )
}

