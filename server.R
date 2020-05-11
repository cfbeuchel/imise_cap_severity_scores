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
          title="Information",{
            # TODO Add Format Info ----
          }
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
            DT::dataTableOutput("data.preview.table",)
          }
        ),
        tabPanel(
          title = "Data Example",{
            DT::dataTableOutput("data.example.table")
            # TODO add example data----
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
          selectInput("psi_ph", "pH", choices = NULL), # TODO: was ist das? ----
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
  # Data preview Box
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
  
  # TODO: SIRS Panel ----
  
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
          # selectInput("sirs_event", "Patient ID", choices = NULL),
          # heart rate
          # respiratory rate
          # selectInput("sirs_resprate", "Respiratory rate [breaths/min]", choices = NULL),
          # leucos
          # suspected infection
          # organ failure
          # Severe sepsis with hypotension, despite adequate fluid resuscitation
          
          # temperature
          selectInput("sirs_tempmin", "Minimum Temperature [°C]", choices = NULL),
          selectInput("sirs_tempmax", "Maximum Temperature [°C]", choices = NULL),
          selectInput("sirs_confusion", "Altered mental status (yes=1 no=0)", choices = NULL),
          
          # sysBP
          selectInput("sirs_bpsys", "Systolic blood pressure [mmHg]", choices = NULL),

          
          # selectInput("sirs_age", "Age [years]", choices = NULL),
          # selectInput("sirs_sex", "Sex (female=1 male=0)", choices = NULL),
          # selectInput("sirs_pulse", "Pulse [beats/min]", choices = NULL),
          # selectInput("sirs_tumor", "Neoplastic disease (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_heart", "Congestive heart failure history (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_cerebo", "Cerebrovascular disease history (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_renal", "Renal disease history  (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_liver", "Liver disease history (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_nursehome", "Nursing home resident (yes=1 no=0)", choices = NULL),
          # selectInput("sirs_ph", "pH", choices = NULL), # TODO: was ist das?
          # selectInput("sirs_bun", "BUN [mmol/L]", choices = NULL),
          # selectInput("sirs_snat", "Snat (Sodium) [mmol/L]", choices = NULL),
          # selectInput("sirs_gluc", "Glucose [mmol/L]", choices = NULL),
          # selectInput("sirs_haem", "Hematocrit [%]", choices = NULL),
          # selectInput("sirs_apo2", "Partial pressure of oxygen [mmHg]", choices = NULL),
          # selectInput("sirs_pleu", "Pleural effusion on x-ray (yes=1 no=0)", choices = NULL),
          tags$hr()
          
        )
      )
    )
  })
  
  # SIRS Data Preview Box
  
  # TODO: quickSOFA Panel ----
  
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
  
  # TODO: Halm Panel ----
  # TODO: SCAP Panel ----
  # TODO: smartCOP Panel ----
  
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
  
  observeEvent(input$input.data, {
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
  
  observeEvent(input$input.data, {
    
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
    
    inputid.sirs <- c() # TODO: Add input IDs ----
    
    inputid.quicksofa <- c("quicksofa_id","quicksofa_respratemin",
                           "quicksofa_respratemax","quicksofa_bpsys", 
                           "quicksofa_confusion","quicksofa_gcs")
    
    
    inputid.halm <- c() # TODO: Add input IDs ----
    inputid.scap <- c() # TODO: Add input IDs ----
    inputid.smartcop <- c() # TODO: Add input IDs ----
    
    
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
      
      # silent check for all inputs
      # req({
      #   input$psi_id
      #   input$psi_age 
      #   input$psi_sex 
      #   input$psi_confusion 
      #   input$psi_pulse 
      #   input$psi_resprate 
      #   input$psi_bpsys
      #   input$psi_tempmin
      #   input$psi_tempmax
      #   input$psi_tumor 
      #   input$psi_heart 
      #   input$psi_cerebo 
      #   input$psi_renal
      #   input$psi_liver
      #   input$psi_nursehome 
      #   input$psi_ph
      #   input$psi_bun
      #   input$psi_snat
      #   input$psi_gluc 
      #   input$psi_haem 
      #   input$psi_apo2 
      #   input$psi_pleu
      # })
      
      # calculate the score and return it
      score <- tryCatch(
        {
          psi_simple(
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
          
        }, error = function(e) {
          
          return(
            paste0(
              "Error in calculation! Please check your input! Error message:",
              as.character(e)))          
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = dat[[input$psi_id]],
        PSI = score
      )
      
    # SIRS ----
    } else if (menu.select=="subtab_sirs"){
      
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
          
          return(
            paste0(
              "Error in calculation! Please check your input! Error message:",
              as.character(e)))          
        }
      )
      
      # create result table
      res <- data.table(
        patient.id = dat[[input$quicksofa_id]],
        qSOFA = score
      )
      
      # Halm ----
    } else if (menu.select=="subtab_halm"){
      
      # SCAP ----
    } else if (menu.select=="subtab_scap"){
      
      # smartCOP ----
    } else if (menu.select=="subtab_smartcop"){
      
    } else {
      
      res <- data.table(" " = "Please choose a score from the menu on the left before pressing the 'Calculate Score'-button.")
      
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