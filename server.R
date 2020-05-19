server <- function(input, output, session) {
  
  # Setup ----
  
  # for storage
  values <- reactiveValues(dat=NULL,
                           res = list("Nothing to download, yet!"))
  
  # UI - Startup message ----
  
  observeEvent("", {
    showModal(
      modalDialog(
        includeHTML("www/popup_text.html"),
        easyClose = TRUE,
        footer = tagList(
          actionButton(
            inputId = "intro",
            label = "Quick Start",
            icon = icon("info-circle")
          )
        )
      ))
    
    # load and display example data format
    values$example.data <- fread("data/exampleData.csv")
  })
  
  # close startup message when pressing the button
  observeEvent(input$intro, {
    removeModal()
  })
  
  # UI - Calculate Score Button ----
  
  # watch for the calculation button and run the specific analysis
  observeEvent(
    input$run.calculate,{
      
      # Feedback dialog
      showModal(
        modalDialog(
          div(
            style="text-align: center;padding: 20px 20px",
            p("The selected score has been calculated! Please see the 'Results'-panel.")
          ),
          easyClose = TRUE,
          footer = actionButton(
            class="intro",
            inputId = "gotoresults",
            label = "Go To Results",
            icon = icon("poll")
          )
        ))
    })
  
  # After showing Info Button -> Go to results Panel
  observeEvent(input$gotoresults,{
    removeModal()
    show("panel_results")
    hide("panel_format")
    hide("panel_upload")
    hide("panel_psi")
    hide("panel_sirs")
    hide("panel_quicksofa")
    hide("panel_halm")
    hide("panel_scap")
    hide("panel_smartcop")
  })
  
  # PANELS ----
  
  # Upload Panel ----
  
  # Is initial but also will return to the (initial) upload panel when pressing the Upload button
  
  # Data Upload Box
  output$box_upload <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_upload",
        width = NULL,
        height = 320,
        tabPanel(
          
          title = "Data Upload",
          
          # dataTableOutput('mytable')
          fileInput('input.data', 
                    'Select a file to upload',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                    )),
          # Input: Select separator
          radioButtons("sep.data",
                       "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          
          # Input: Select quotes
          radioButtons("quote.data", "Quote",
                       choices = c(None = "",
                                   "Double Quote" = '"',
                                   "Single Quote" = "'"),
                       selected = '"'),
          # Horizontal line
          tags$hr()
          
        ),
        tabPanel(
          title="Information",
          helpText("Test the functionality of the App using the example data. The example data can be used to calculate the Quick SOFA score."),
          actionButton(inputId = "use.example.dat",label = "Use Example Data"),
          tags$hr(),
          p("The data should be organized as a table with a row for each sample and a column for each feature. It is neccessary to specify an ID column."),
          p("Most data will be numeric. In case of a binary variable, necessary numeric dummy-encoding is stated in the column selection box (yes=1, no=0)"),
          tags$hr()
        )
      )
    )
  })
  
  # Data preview Box
  output$box_preview <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview",
        width = NULL,
        height = NULL,
        tabPanel(
          title = "Upload Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table")
          }
        ),
        tabPanel(
          title = "Data Example",{
            DT::dataTableOutput("data.example.table")
          }
        )
      )
    )
  })
  
  
  # SCORE PANEL BOXES ----
  
  # Show the different Score panels when selecting a submenu
  
  # PSI Panel ----
  
  # PSI Selection Box
  output$box_select_psi <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_psi",
        width = NULL,
        height = 640,
        tabPanel(
          title = "PSI",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the PSI score."),
          tags$hr(),
          selectInput("psi_id", "Patient ID", choices = NULL),
          selectInput("psi_age", "Age [years]", choices = NULL),
          selectInput("psi_sex", "Sex (female=1 male=0)", choices = NULL),
          selectInput("psi_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("psi_pulse", "Pulse [beats/min]", choices = NULL),
          selectInput("psi_resprate", "Respiratory rate [breaths/min]", choices = NULL),
          selectInput("psi_bpsys", "Systolic blood pressure [mmHg]", choices = NULL),
          selectInput("psi_tempmin", "Minimum Temperature [°C]", choices = NULL),
          selectInput("psi_tempmax", "Maximum Temperature [°C]", choices = NULL),
          selectInput("psi_tumor", "Neoplastic disease (yes=1 no=0)", choices = NULL),
          selectInput("psi_heart", "Congestive heart failure history (yes=1 no=0)", choices = NULL),
          selectInput("psi_cerebo", "Cerebrovascular disease history (yes=1 no=0)", choices = NULL),
          selectInput("psi_renal", "Renal disease history  (yes=1 no=0)", choices = NULL),
          selectInput("psi_liver", "Liver disease history (yes=1 no=0)", choices = NULL),
          selectInput("psi_nursehome", "Nursing home resident (yes=1 no=0)", choices = NULL),
          selectInput("psi_ph", "Arterial pH", choices = NULL),
          selectInput("psi_bun", "BUN [mmol/L]", choices = NULL),
          selectInput("psi_snat", "Snat (Sodium) [mmol/L]", choices = NULL),
          selectInput("psi_gluc", "Glucose [mmol/L]", choices = NULL),
          selectInput("psi_haem", "Hematocrit [%]", choices = NULL),
          selectInput("psi_apo2", "Partial pressure of oxygen [mmHg]", choices = NULL),
          selectInput("psi_pleu", "Pleural effusion on x-ray (yes=1 no=0)", choices = NULL),
          tags$hr()
          
        )
      )
    )
  })
  
  # PSI Data Preview Box
  output$box_preview_psi <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_psi",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.psi")
          }
        )
      )
    )
  })
  
  # SIRS Panel ----
  
  # SIRS Selection Box
  output$box_select_sirs <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_sirs",
        width = NULL,
        height = 640,
        tabPanel(
          title = "SIRS",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the SIRS score."),
          tags$hr(),
          selectInput("sirs_id", "Patient ID", choices = NULL),
          selectInput("sirs_tempmin", "Minimum Temperature [°C]", choices = NULL),
          selectInput("sirs_tempmax", "Maximum Temperature [°C]", choices = NULL),
          selectInput("sirs_heartratemin", "Heart rate [beats/min]", choices = NULL),
          selectInput("sirs_respratemax", "Respiratory rate [breaths/min]", choices = NULL),
          selectInput("sirs_pco2", "Arterial partial pressure of CO2 [kPa]", choices = NULL),
          selectInput("sirs_leukmin", "Minimum leucocytes [count/cm³]", choices = NULL),
          selectInput("sirs_leukmax", "Maximum leucocytes [count/cm³]", choices = NULL),
          selectInput("sirs_bandneutr", "Banded neutrophils [count/µL]", choices = NULL),
          selectInput("sirs_segneutr", "Segmented neutrophils [count/µL]", choices = NULL),
          selectInput("sirs_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("sirs_thrombmin", "Minimum thrombocytes [count/µL]", choices = NULL),
          selectInput("sirs_thrombprevmin", "Previous day minimum thrombocytes [count/µL]", choices = NULL),
          selectInput("sirs_oxiind", "Minimum oxygenation (Horowitz) index [kPA]", choices = NULL),
          selectInput("sirs_chronlung", "Chronic lung disease (yes=1, no=0)", choices = NULL),
          selectInput("sirs_diur", "Diuresis [ml/day]", choices = NULL),
          selectInput("sirs_weight", "Weight [kg]", choices = NULL),
          selectInput("sirs_bemin", "Base excess", choices = NULL),
          selectInput("sirs_bpsys", "Systolic blood pressure [mmHg]", choices = NULL),
          selectInput("sirs_map", "Mean arterial pressure [kPa]", choices = NULL),
          selectInput("sirs_kate", "Treatment with catecholamines (yes=1, no=0)", choices = NULL),
          tags$hr()
        )
      )
    )
  })
  
  # SIRS Data Preview Box
  output$box_preview_sirs <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_sirs",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.sirs")
          }
        )
      )
    )
  })
  
  # quickSOFA Panel ----
  
  # quickSOFA Selection Box
  output$box_select_quicksofa <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_quicksofa",
        width = NULL,
        height = 640,
        tabPanel(
          title = "Quick SOFA",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the Quick SOFA score."),
          tags$hr(),
          selectInput("quicksofa_id", "Patient ID", choices = NULL),
          selectInput("quicksofa_respratemin", "Minimum Respiratory rate [breaths/min]", choices = NULL),
          selectInput("quicksofa_respratemax", "Maximum Respiratory rate [breaths/min]", choices = NULL),
          selectInput("quicksofa_bpsys", "Minimum Systolic blood pressure [mmHg]", choices = NULL),
          selectInput("quicksofa_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("quicksofa_gcs", "GCS (Glasgow Coma Scale)", choices = NULL),
          tags$hr()
          
        )
      )
    )
  })
  
  # quickSOFA Data Preview Box
  output$box_preview_quicksofa <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_quicksofa",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.quicksofa")
          }
        )
      )
    )
  })
  
  # Halm Panel ----
  
  # Halm Selection Box
  output$box_select_halm <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_halm",
        width = NULL,
        height = 640,
        tabPanel(
          title = "Halm",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the Halm score."),
          tags$hr(),
          selectInput("halm_id", "Patient ID", choices = NULL),
          selectInput("halm_heartratemax", "Heart rate [beats/min]", choices = NULL),
          selectInput("halm_bpsys", "Minimum Systolic blood pressure [mmHg]", choices = NULL),
          selectInput("halm_respratemin", "Respiratory rate [breaths/min]", choices = NULL),
          selectInput("halm_o2p", "Minimum oxygen saturation [%]", choices = NULL),
          selectInput("halm_apo2", "Partial pressure of oxygen [mmHg]", choices = NULL),
          selectInput("halm_mecvent", "Mechanical ventilation (yes=1, no=0)", choices = NULL),
          selectInput("halm_oxther", "Oxygen therapy (yes=1, no=0)", choices = NULL),
          selectInput("halm_tempmax", "Maximum Temperature [°C]", choices = NULL),
          selectInput("halm_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("halm_gcs", "GCS (Glasgow Coma Scale)", choices = NULL),
          tags$hr()
        )
      )
    )
  })
  
  # Halm Data Preview Box
  output$box_preview_halm <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_halm",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.halm")
          }
        )
      )
    )
  })
  
  # SCAP Panel ----
  
  # SCAP Selection Box
  output$box_select_scap <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_scap",
        width = NULL,
        height = 640,
        tabPanel(
          title = "SCAP",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the SCAP score."),
          tags$hr(),
          selectInput("scap_id", "Patient ID", choices = NULL),
          
          # arterial ph
          selectInput("scap_artph", "Artierial pH", choices = NULL),
          
          # bpsys
          selectInput("scap_bpsys", "Minimum Systolic blood pressure [mmHg]", choices = NULL),
          
          # resp rate
          selectInput("scap_respratemin", "Minimum Respiratory rate [breaths/min]", choices = NULL),
          selectInput("scap_respratemax", "Maximum Respiratory rate [breaths/min]", choices = NULL),
          
          # confusion
          selectInput("scap_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("scap_gcs", "GCS (Glasgow Coma Scale)", choices = NULL),
          
          # BUN (blood urea nitrogen)
          selectInput("scap_bun", "BUN [mmol/L]", choices = NULL),
          
          # Age ≥80 years
          selectInput("scap_age", "Age [years]", choices = NULL),
          
          # PaO2 <54 mmHg (or PaO2/FiO2 <250 mmHg) partial O2 pressure
          selectInput("scap_apo2", "Horowitz-Index (PaO2/FiO2) [mmHg]", choices = NULL),
          
          # Multilobar/bilateral X-ray (yes=1, no=0)
          selectInput("scap_multl", "Multilobar/bilateral X-ray (yes=1 no=0)", choices = NULL),
          
          tags$hr()
          
        )
      )
    )
  })
  
  # SCAP Data Preview Box
  output$box_preview_scap <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_scap",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.scap")
          }
        )
      )
    )
  })
  
  # smartCOP Panel ----
  
  # SMART-COP Selection Box
  output$box_select_smartcop <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll",
      tabBox(
        id = "box_select_smartcop",
        width = NULL,
        height = 640,
        tabPanel(
          title = "SMART-COP",
          helpText("Please select the columns in your data that encode the necessary information for the computation of the SMART-COP score."),
          tags$hr(),
          selectInput("smartcop_id", "Patient ID", choices = NULL),
          
          # age
          selectInput("smartcop_age", "Age [years]", choices = NULL),
          
          # multl
          selectInput("smartcop_multl", "Multilobar/bilateral X-ray (yes=1 no=0)", choices = NULL),
          
          # albumin
          selectInput("smartcop_alb", "Albumin [g/L]", choices = NULL),
          
          # resprate
          selectInput("smartcop_respratemin", "Minimum Respiratory rate [breaths/min]", choices = NULL),
          selectInput("smartcop_respratemax", "Maximum Respiratory rate [breaths/min]", choices = NULL),
          
          # tachycardia
          selectInput("smartcop_heartratemin", "Minimum heart rate [beats/min]", choices = NULL),
          selectInput("smartcop_heartratemax", "Maximum heart rate [beats/min]", choices = NULL),
          
          # confusion & gcs
          selectInput("smartcop_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          selectInput("smartcop_gcs", "GCS (Glasgow Coma Scale)", choices = NULL),
          
          # pao2
          selectInput("smartcop_apo2", "Partial pressure of oxygen [mmHg]", choices = NULL),
          
          # ph
          selectInput("smartcop_artph", "Artierial pH", choices = NULL),
          
          # sysp
          selectInput("smartcop_bpsys", "Minimum Systolic blood pressure [mmHg]", choices = NULL),
          
          tags$hr()
        )
      )
    )
  })
  
  # SMART-COP Data Preview Box
  output$box_preview_smartcop <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_preview_smartcop",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Data Preview", {
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table.smartcop")
          }
        )
      )
    )
  })
  
  # Results Panel ----
  
  # Move to the results panel when pressing the "Calculate Score Button" as well when the Result menu entry is selected
  
  # Results Download Box
  output$box_resultsdownload <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_resultsdownload",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Downloads",
          helpText("Download the results here:"),
          tags$hr(),
          downloadButton("download.res", "Download", class = "butt"),
          tags$hr()
        )
      )
    )
  })
  
  # Result preview Box
  output$box_resultsview <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_resultsview",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Results",
          DT::dataTableOutput("result.preview")
        )
      )
    )
  })
  
  # DYNAMIC UI ----
  
  # Init - show the upload panel by default
  observeEvent("" ,{
    show("panel_upload")
    hide("panel_results")
    hide("panel_psi")
    hide("panel_sirs")
    hide("panel_quicksofa")
    hide("panel_halm")
    hide("panel_scap")
    hide("panel_smartcop")
    
  }, once = TRUE)
  
  # UI based on menu selection
  observeEvent(input$menu, {
    
    if(input$menu=="tab_results"){
      hide("panel_upload")
      show("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    } else if(input$menu == "tab_data"){
      show("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    } else if(input$menu == "subtab_psi"){
      hide("panel_upload")
      hide("panel_results")
      show("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
      
    } else if(input$menu == "subtab_sirs"){
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      show("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
      
    } else if(input$menu == "subtab_quicksofa"){
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      show("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
      
    } else if(input$menu == "subtab_halm"){
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      show("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
      
    } else if(input$menu == "subtab_scap"){
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      show("panel_scap")
      hide("panel_smartcop")
      
    } else if(input$menu == "subtab_smartcop"){
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      show("panel_smartcop")
    }
  }, once = FALSE)
  
  # REACTIVE PROCESSES ----
  
  # Example Data ----
  
  # render the example data
  output$data.example.table <- DT::renderDataTable({values$example.data})
  
  # Data Upload ----
  
  observeEvent(
    input$input.data, {
      req(input$input.data)
      tryCatch(
        {
          # read the input data
          values$dat <- fread(input$input.data$datapath,
                              sep = input$sep.data,
                              quote = input$quote.data)
          
          # save the input data in the reactive value 
          #  <- input.data
          
          # return the input data
          # input.data
          
        }, error = function(e) {
          stop(safeError(e))
        }
      )
    })
  
  # execute reactive reading of upload data
  output$data.preview.table <- DT::renderDataTable({values$dat})
  output$data.preview.table.psi <- DT::renderDataTable({values$dat})
  output$data.preview.table.sirs <- DT::renderDataTable({values$dat})
  output$data.preview.table.quicksofa <- DT::renderDataTable({values$dat})
  output$data.preview.table.halm <- DT::renderDataTable({values$dat})
  output$data.preview.table.scap <- DT::renderDataTable({values$dat})
  output$data.preview.table.smartcop <- DT::renderDataTable({values$dat})
  
  # Update Column Selection after Upload ----
  
  observeEvent({
    input$input.data
    input$use.example.dat
  },{
    
    if(input$use.example.dat>=1 & is.null(input$input.data)){
      values$dat <- values$example.data
    }
    
    # get the data 
    dat <- values$dat
    
    # collect all inputIDs
    inputid.psi <- c("psi_id", "psi_age", "psi_sex", "psi_confusion",
                     "psi_pulse", "psi_resprate", "psi_bpsys",
                     "psi_tempmin", "psi_tempmax", "psi_tumor", 
                     "psi_heart", "psi_cerebo", "psi_renal",
                     "psi_liver", "psi_nursehome", "psi_ph",
                     "psi_bun", "psi_snat", "psi_gluc",
                     "psi_haem", "psi_apo2", "psi_pleu")
    
    inputid.sirs <- c(
      "sirs_id",
      "sirs_tempmin",
      "sirs_tempmax", 
      "sirs_heartratemin",
      "sirs_respratemax",
      "sirs_pco2",
      "sirs_leukmin",
      "sirs_leukmax",
      "sirs_bandneutr",
      "sirs_segneutr", 
      "sirs_confusion",
      "sirs_thrombmin",
      "sirs_thrombprevmin", 
      "sirs_oxiind", 
      "sirs_chronlung",
      "sirs_diur",
      "sirs_weight", 
      "sirs_bemin",
      "sirs_bpsys", 
      "sirs_map",
      "sirs_kate")
    
    inputid.quicksofa <- c("quicksofa_id","quicksofa_respratemin",
                           "quicksofa_respratemax","quicksofa_bpsys", 
                           "quicksofa_confusion","quicksofa_gcs")
    
    inputid.halm <- c(
      "halm_id", 
      "halm_heartratemax", 
      "halm_bpsys", 
      "halm_respratemin", 
      "halm_o2p", 
      "halm_apo2", 
      "halm_mecvent",
      "halm_oxther", 
      "halm_tempmax", 
      "halm_confusion", 
      "halm_gcs")
    
    inputid.scap <- c("scap_id", "scap_respratemin", "scap_respratemax", 
                      "scap_bpsys", "scap_confusion", "scap_gcs", 
                      "scap_age", "scap_apo2", "scap_multl", 
                      "scap_bun", "scap_artph")
    
    inputid.smartcop <- c("smartcop_id", "smartcop_respratemin", "smartcop_respratemax", 
                          "smartcop_heartratemin", "smartcop_heartratemax", "smartcop_alb",
                          "smartcop_bpsys", "smartcop_confusion", "smartcop_gcs", 
                          "smartcop_age", "smartcop_apo2", "smartcop_multl", 
                          "smartcop_artph")
    
    # join ids
    my.inputid <- c(inputid.psi, 
                    inputid.sirs,
                    inputid.quicksofa,
                    inputid.halm,
                    inputid.scap,
                    inputid.smartcop)
    
    # update all ids
    for(i in my.inputid){
      updateSelectInput(
        session, 
        i, 
        choices = c(NULL,names(dat)),
        selected = names(dat)[pmatch(i,names(dat),nomatch = NULL)]
      )
    }
  })
  
  # SCORE CALCULATION ----
  
  observeEvent(input$run.calculate,{
    
    # render output text in case not all colums are provided
    output$result.preview <- renderText(
      validate(
        need(
          expr = values$dat,
          message = "No input data provided! Please upload your data!")
      )
    )
    
    # get the menu 
    menu.select <- input$menu
    
    # data
    dat <- isolate(values$dat)
    
    # PSI ----
    if(menu.select=="subtab_psi"){
      
      # calculate the score and return it
      score <- tryCatch(
        {
          data.table(
            id = dat[[input$psi_id]],
            psi = psi_simple(
              age = dat[,.SD,.SDcols=input$psi_age],
              gender = dat[,.SD,.SDcols=input$psi_sex],
              verwirrt = dat[,.SD,.SDcols=input$psi_confusion],
              hfrq.max = dat[,.SD,.SDcols=input$psi_pulse],
              afrq.max = dat[,.SD,.SDcols=input$psi_resprate],
              sysbp.min = dat[,.SD,.SDcols=input$psi_bpsys],
              temp.min = dat[,.SD,.SDcols=input$psi_tempmin],
              temp.max = dat[,.SD,.SDcols=input$psi_tempmax],
              tumor = dat[,.SD,.SDcols=input$psi_tumor],
              herz = dat[,.SD,.SDcols=input$psi_heart],
              cerebro = dat[,.SD,.SDcols=input$psi_cerebo],
              renal = dat[,.SD,.SDcols=input$psi_renal],
              liver = dat[,.SD,.SDcols=input$psi_liver],
              nurse.home = dat[,.SD,.SDcols=input$psi_nursehome],
              art.ph.min = dat[,.SD,.SDcols=input$psi_ph],
              bun = dat[,.SD,.SDcols=input$psi_bun],
              snat = dat[,.SD,.SDcols=input$psi_snat],
              gluk = dat[,.SD,.SDcols=input$psi_gluc],
              haemkrt = dat[,.SD,.SDcols=input$psi_haem],
              apo2.min = dat[,.SD,.SDcols=input$psi_apo2],
              pleu_erg = dat[,.SD,.SDcols=input$psi_pleu])
          )
          
        }, error = function(e) {
          
          return(
            data.table(
              id = NA,
              psi = paste0(
                "Error in calculation! Please check your input! Error message:",
                as.character(e)
              )
            )
          )        
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$id,
        PSI = score$psi
      )
      
      # SIRS ----
    } else if (menu.select=="subtab_sirs"){
      
      
      # calculate the score and return it
      score <- tryCatch(
        {
          
          score <- sirs_simple(
            temp.min = dat[[input$sirs_tempmin]],
            temp.max = dat[[input$sirs_tempmax]],
            hfrq.max = dat[[input$sirs_heartratemax]],
            afrq.max = dat[[input$sirs_respratemax]],
            pco2 = dat[[input$sirs_pco2]],
            leuko_min = dat[[input$sirs_leukmin]],
            leuko_max = dat[[input$sirs_leukmax]],
            stkern.neutro = dat[[input$sirs_bandneutr]],
            smkern.neutro = dat[[input$sirs_segneutr]],
            verwirrt = dat[[input$sirs_confusion]],
            thrombo_min = dat[[input$sirs_thrombmin]],
            thrombo.daybefore = dat[[input$sirs_thrombprevmin]],
            oxi.ind = dat[[input$sirs_oxiind]],
            chr.lunge = dat[[input$sirs_chronlung]],
            diur = dat[[input$sirs_diur]],
            gewicht = dat[[input$sirs_weight]],
            bemin = dat[[input$sirs_bemin]],
            sysbp.min = dat[[input$sirs_bpsys]],
            map = dat[[input$sirs_map]],
            kate = dat[[input$sirs_kate]] 
          )
          
          data.table(
            id = dat[[input$sirs_id]],
            sirs = score$infec.septic.servsept,
            shock = score$shock
          )
          
        }, error = function(e) {
          
          return(
            data.table(
              id=NA,
              sirs=paste0(
                "Error in calculation! Please check your input! Error message:",
                as.character(e)),
              shock=NA
            )
          )
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$id,
        SIRS = score$sirs,
        septic.shock = score$shock
        
      )
      
      # quickSOFA ----
    } else if (menu.select=="subtab_quicksofa"){
      
      # calculate the score and return it
      score <- tryCatch(
        {
          quicksofa_simple(
            ID = dat[,.SD,.SDcols=input$quicksofa_id],
            BPSysMin = dat[,.SD,.SDcols=input$quicksofa_bpsys],
            RespRateMin = dat[,.SD,.SDcols=input$quicksofa_respratemin],
            RespRateMax = dat[,.SD,.SDcols=input$quicksofa_respratemax],
            Confusion = dat[,.SD,.SDcols=input$quicksofa_confusion],
            GCS = dat[,.SD,.SDcols=input$quicksofa_gcs]
          )
        }, error = function(e) {
          
          data.table(
            PATSTUID=NA,
            qSOFA=paste0(
              "Error in calculation! Please check your input! Error message:",
              as.character(e))
          )
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$PATSTUID,
        qSOFA = score$qSOFA
      )
      
      # Halm ----
    } else if (menu.select=="subtab_halm"){
      
      # calculate the score and return it
      score <- tryCatch(
        {
          data.table(
            halm = HalmScore_simple(
              hfrq.max = dat[[input$halm_heartratemax]],
              sysbp.min = dat[[input$halm_bpsys]],
              afrq.max = dat[[input$halm_respratemin]],
              o2p.min = dat[[input$halm_o2p]],
              apo2.min = dat[[input$halm_apo2]],
              bea = dat[[input$halm_mecvent]],
              sauerst = dat[[input$halm_oxther]],
              temp.max = dat[[input$halm_tempmax]],
              verwirrt = dat[[input$halm_confusion]],
              gcs = dat[[input$halm_gcs]]
            ),
            id = dat[[input$halm_id]]
          )
          
        }, error = function(e) {
          
          return(
            data.table(
              id=NA,
              halm=paste0(
                "Error in calculation! Please check your input! Error message:",
                as.character(e)
              )
            )
          )
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$id,
        Halm = score$halm
      )
      
      # SCAP ----
    } else if (menu.select=="subtab_scap"){
      
      # calculate the score and return it
      score <- tryCatch(
        {
          scap_simple(
            ID = dat[,.SD,.SDcols=input$scap_id],
            BPSysMin = dat[,.SD,.SDcols=input$scap_bpsys],
            RespRateMin = dat[,.SD,.SDcols=input$scap_respratemin],
            RespRateMax = dat[,.SD,.SDcols=input$scap_respratemax],
            Confusion = dat[,.SD,.SDcols=input$scap_confusion],
            GCS = dat[,.SD,.SDcols=input$scap_gcs],
            ArtpH = dat[,.SD,.SDcols=input$scap_artph],
            BUN = dat[,.SD,.SDcols=input$scap_bun],
            Age = dat[,.SD,.SDcols=input$scap_age],
            PaO2 = dat[,.SD,.SDcols=input$scap_apo2],
            MultLobXRay = dat[,.SD,.SDcols=input$scap_multl]
          )
        }, error = function(e) {
          
          data.table(
            PATSTUID=NA,
            SCAP=paste0(
              "Error in calculation! Please check your input! Error message:",
              as.character(e))
          )
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$PATSTUID,
        SCAP = score$SCAP
      )
      
      # smartCOP ----
    } else if (menu.select=="subtab_smartcop"){
      
      # calculate the score and return it
      score <- tryCatch(
        {
          smartcop_simple(
            ID = dat[,.SD,.SDcols=input$smartcop_id],
            BPSysMin = dat[,.SD,.SDcols=input$smartcop_bpsys],
            RespRateMin = dat[,.SD,.SDcols=input$smartcop_respratemin],
            RespRateMax = dat[,.SD,.SDcols=input$smartcop_respratemax],
            HeartRateMin = dat[,.SD,.SDcols=input$smartcop_heartratemin],
            HeartRateMax = dat[,.SD,.SDcols=input$smartcop_heartratemax],
            Confusion = dat[,.SD,.SDcols=input$smartcop_confusion],
            GCS = dat[,.SD,.SDcols=input$smartcop_gcs],
            ArtpH = dat[,.SD,.SDcols=input$smartcop_artph],
            Alb = dat[,.SD,.SDcols=input$smartcop_alb],
            Age = dat[,.SD,.SDcols=input$smartcop_age],
            PaO2 = dat[,.SD,.SDcols=input$smartcop_apo2],
            MultLobXRay = dat[,.SD,.SDcols=input$smartcop_multl]
          )
        }, error = function(e) {
          
          data.table(
            PATSTUID=NA,
            smartCOP=paste0(
              "Error in calculation! Please check your input! Error message:",
              as.character(e))
          )
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = score$PATSTUID,
        smartCOP = score$smartCOP
      )
      
    } else {
      
      res <- data.table(
        ID = NA,
        Score = "Please choose a score from the menu on the left before pressing the 'Calculate Score'-button."
        )
      
    }
    
    # save output
    values$res <- res
    
    # display results
    output$result.preview <- DT::renderDataTable({res})
    
  })
  
  # DOWNLOAD ----
  
  # download data
  output$download.res <- downloadHandler(
    filename = "cap_severity_score_results.csv",
    content = function(file) {
      fwrite(isolate(values$res), file)
    }
  )
  
}