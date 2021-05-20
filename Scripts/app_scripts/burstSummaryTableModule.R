### Summary Table Module

# https://shiny.rstudio.com/articles/modules.html

burstSummaryTableUI <- function(
  id,
  bParamsDF
){
  ns <- NS(id)
  tagList(
    fileInput(
      ns("bParamsFile"),
      label = "Select Burst Params File",
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv",
        ".xlsx")
    ),
    
    textInput(
      ns("sheetName"),
      label = "Enter Sheet Name",
      value = "Sheet1"
    ),
    
    h2("Generate summary data for variables"),
    
    # filterDFUI(ns("filterDF"), bParamsDF),
    
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
    
    #create a space for the data table output
    shiny::dataTableOutput(ns("summarydf")),
    
    ### ANOVAs ----
    h2("ANOVAs"),
    
    # fluidRow(
    #   column(
    #     3,
    #     varSelectInput(
    #       ns("ANOVA_var"),
    #       "Select variable for ANOVA:",
    #       data = bParamsDF %>%
    #         select(bn:ssFlags),
    #       selected = "bf"
    #     )
    #   )
    # ),
    
    htmlOutput(ns("ANOVA_table")),
    
    DTOutput(ns("burstTable"))
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
      
      output$burstTable <- renderDT({
        bParamsDF()
      })
      
    }
  )
}

