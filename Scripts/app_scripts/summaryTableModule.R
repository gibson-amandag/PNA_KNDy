### Summary Table Module

# https://shiny.rstudio.com/articles/modules.html

summaryTableUI <- function(
  id,
  KNDyDATA
){
  ns <- NS(id)
  tagList(
    
    h2("Generate summary data for variables"),
    
    # instructions ui
    instructionsSumTableUI(ns("instructions")),
    
    #Summary information
    fluidRow(
      #column 1
      column(
        3,
        #grouping variables
        varSelectInput(
          inputId = ns("group_vars"),
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
          inputId = ns("var_toSummarize"),
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
          inputId = ns("dataset3"), 
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
          inputId = ns("exclude3"),
          label = "Exclude marked cells",
          value = TRUE
        )
      ),
      
      #column 4
      column(
        3,
        #Which activity levels to include
        radioButtons(
          inputId = ns("firing3"), 
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
    dataTableOutput(ns("summarydf")),
    
    ### ANOVAs ----
    h2("ANOVAs"),
    
    fluidRow(
      column(
        3,
        radioButtons(
          inputId = ns("whoRecordedSel"), 
          label = "Select recording experimenter:",
          choices = list(
            "Both",
            "Amanda",
            "Jenn"
          ),
          selected = "Both"
        ),
      ),
      
      column(
        3,
        varSelectInput(
          ns("ANOVA_var"),
          "Select variable for ANOVA:",
          data = KNDyDATA %>%
            select(SpontAvgFiring:MaxBurstWindow_senktide),
          selected = "SpontAvgFiring"
        )
      )
    ),
    
    htmlOutput(ns("ANOVA_table")),
    
    h4("By Who Recorded"),
    htmlOutput(ns("ANOVA_table_whoRecorded")),
    
    h4("By Who Sliced"),
    htmlOutput(ns("ANOVA_table_who"))
    
  )
}


summaryTableServer <- function(
  id,
  KNDyDATA,
  KNDyDATA_adult,
  KNDyDATA_juv,
  KNDy_VarNames
){
  moduleServer(
    id,
    function(input, output, session) {
      instructionsSumTableServer("instructions")
      
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
      
      output$ANOVA_table <- renderText({
        data <- KNDyDATA
        
        if(input$whoRecordedSel == "Amanda"){
          data <- data %>%
            filter(WhoRecorded == "Amanda")
        }
        
        if(input$whoRecordedSel == "Jenn"){
          data <- data %>%
            filter(WhoRecorded == "Jenn")
        }
          
        # The makeTreatandAgeContrasts function excludes cells
        KNDyDATA_contrasts <- makeTreatandAgeContrasts(data)

        Model <- lm_byTreatxAge(input$ANOVA_var, KNDyDATA_contrasts)

        ANOVA_table <- makeANOVA_TreatAge(Model)
        ANOVA_table %>%
          kable_styling(
            font_size = 18,
            bootstrap_options = c("striped"), full_width = TRUE
          )
      })
      
      output$ANOVA_table_whoRecorded <- renderText({
        
        data <- switch(
          input$dataset3,
          "All" = KNDyDATA,
          "Adults" = KNDyDATA_adult,
          "Juveniles" = KNDyDATA_juv
          
        )
        
        # The makeTreatandAgeContrasts function excludes cells
        KNDyDATA_contrasts <- makeTreatandWhoRecordedContrasts(data)
        
        Model <- lm_byTreatxWhoRecorded(input$ANOVA_var, KNDyDATA_contrasts)
        
        ANOVA_table <- makeANOVA_TreatWhoRecorded(Model)
        ANOVA_table %>%
          kable_styling(
            font_size = 18,
            bootstrap_options = c("striped"), full_width = TRUE
          )
      })
      
      output$ANOVA_table_who <- renderText({
        
        
        data <- switch(
          input$dataset3,
          "All" = KNDyDATA,
          "Adults" = KNDyDATA_adult,
          "Juveniles" = KNDyDATA_juv
          
        )
        
        # The makeTreatandAgeContrasts function excludes cells
        KNDyDATA_contrasts <- makeTreatandWhoContrasts(data)
        
        Model <- lm_byTreatxWho(input$ANOVA_var, KNDyDATA_contrasts)
        
        ANOVA_table <- makeANOVA_TreatWho(Model)
        ANOVA_table %>%
          kable_styling(
            font_size = 18,
            bootstrap_options = c("striped"), full_width = TRUE
          )
      })
      
    }
  )
}

