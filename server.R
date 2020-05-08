server <- function(input output session) {
  
  # Setup ----
  
  # for storage
  values <- reactiveValues(dat=NULL) # define dat to be able to validate its existance
  
  # UI - Startup message ----
  
  observeEvent("" {
    showModal(
      modalDialog(
        includeHTML("www/popup_text.html")
        easyClose = TRUE
        footer = tagList(
          actionButton(
            inputId = "intro"
            label = "Quick Start"
            icon = icon("info-circle")
          )
        )
      ))
  })
  
  # close startup message when pressing the button
  observeEvent(input$intro{
    removeModal()
  })
  
  # UI - Calculate Score Button ----
  
  # watch for the calculation button and run the specific analysis
  observeEvent(
    input$run.calculate{
      
      # Feedback dialog
      showModal(
        modalDialog(
          div(
            style="text-align: center;padding: 20px 20px"
            p("The selected score has been calculated! Please see the 'Results'-panel.")
          )
          easyClose = TRUE
          footer = actionButton(
            class="intro"
            inputId = "gotoresults"
            label = "Go To Results"
            icon = icon("poll")
          )
        ))
    })
  
  # After showing Info Button -> Go to results Panel
  observeEvent(input$gotoresults{
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
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_upload"
        width = NULL
        height = 320
        tabPanel(
          title = "Data Upload"
          
          # dataTableOutput('mytable')
          fileInput('input.data' 'Select a file to upload'
                    accept = c(
                      'text/csv'
                      'text/comma-separated-values'
                      'text/tab-separated-values'
                      'text/plain'
                      '.csv'
                      '.tsv'
                    ))
          # Input: Select separator
          radioButtons("sep.data" "Separator"
                       choices = c(Comma = ""
                                   Semicolon = ";"
                                   Tab = "\t")
                       selected = "")
          
          # Input: Select quotes
          radioButtons("quote.data" "Quote"
                       choices = c(None = ""
                                   "Double Quote" = '"'
                                   "Single Quote" = "'")
                       selected = '"')
          # Horizontal line
          tags$hr()
          
        )
      )
    )
  })
  
  # Data preview Box
  output$box_preview <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_preview"
        width = NULL
        height = 320
        tabPanel(
          title = "Data Preview"{
            
            # preview the uploaded data
            DT::dataTableOutput("data.preview.table")
          }
        )
      )
    )
  })
  
  # Format Panel ----
  
  # Data Format Box - Info about necessary data format
  output$box_formatdescription <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_formatdescription"
        width = NULL
        height = 320
        tabPanel(
          title = "Format"
        )
      )
    )
  })
  
  # Format example Box
  output$box_formatexample <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_formatexample"
        width = NULL
        height = 320
        tabPanel(
          title = "Example"
        )
      )
    )
  })
  
  # Menu Input Panel ----
  
  # Show the different Score panels when selecting a submenu
  
  # TODO - Score Panels ----
  
  output$box_select_psi <- renderUI({
    
    div(
      style="position: relative; backgroundColor: #ecf0f5; overflow-y:scroll"
      tabBox(
        id = "box_select_psi"
        width = NULL
        height = 640
        tabPanel(
          title = "PSI"
          helpText("Please select the columns in your data that encode the necessary information for the computation of the PSI score.")
          tags$hr()
          selectInput("psi_id" "Patient ID" choices = NULL)
          selectInput("psi_age" "Age [years]" choices = NULL)
          selectInput("psi_sex" "Sex (female=1 male=0)" choices = NULL)
          selectInput("psi_confusion" "Altered mental status (yes=1 no=0)" choices = NULL)
          selectInput("psi_pulse" "Pulse [beats/min]" choices = NULL)
          selectInput("psi_resprate" "Respiratory rate [breaths/min]" choices = NULL)
          selectInput("psi_bpsys" "Systolic blood pressure [mmHg]" choices = NULL)
          selectInput("psi_tempmin" "Minimum Temperature [°C]" choices = NULL)
          selectInput("psi_tempmax" "Maximum Temperature [°C]" choices = NULL)
          selectInput("psi_tumor" "Neoplastic disease (yes=1 no=0)" choices = NULL)
          selectInput("psi_heart" "Congestive heart failure history (yes=1 no=0)" choices = NULL)
          selectInput("psi_cerebo" "Cerebrovascular disease history (yes=1 no=0)" choices = NULL)
          selectInput("psi_renal" "Renal disease history  (yes=1 no=0)" choices = NULL)
          selectInput("psi_liver" "Liver disease history (yes=1 no=0)" choices = NULL)
          selectInput("psi_nursehome" "Nursing home resident (yes=1 no=0)" choices = NULL)
          selectInput("psi_ph" "pH" choices = NULL) # TODO: was ist das? ----
          selectInput("psi_bun" "BUN [mmol/L]" choices = NULL)
          selectInput("psi_snat" "Snat (Sodium) [mmol/L]" choices = NULL)
          selectInput("psi_gluc" "Glucose [mmol/L]" choices = NULL)
          selectInput("psi_haem" "Hematocrit [%]" choices = NULL)
          selectInput("psi_apo2" "Partial pressure of oxygen [mmHg]" choices = NULL)
          selectInput("psi_pleu" "Pleural effusion on x-ray (yes=1 no=0)" choices = NULL)
          tags$hr()
          
        )
      )
    )
  })
  
  # Results Panel ----
  
  # Move to the results panel when pressing the "Calculate Score Button" as well when the Result menu entry is selected
  
  # Results Download Box
  output$box_resultsdownload <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_resultsdownload"
        width = NULL
        height = 320
        tabPanel(
          title = "Downloads"
        )
      )
    )
  })
  
  # Result preview Box
  output$box_resultsview <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5"
      tabBox(
        id = "box_resultsview"
        width = NULL
        height = 320
        tabPanel(
          title = "Results"
          DT::dataTableOutput("result.preview")
        )
      )
    )
  })
  
  # Dynamic Rendering ----
  
  # Init - show the upload panel by default
  observeEvent("" {
    hide("panel_format")
    show("panel_upload")
    hide("panel_results")
    hide("panel_psi")
    hide("panel_sirs")
    hide("panel_quicksofa")
    hide("panel_halm")
    hide("panel_scap")
    hide("panel_smartcop")
    
  }once = TRUE)
  
  # UI based on menu selection
  observeEvent(input$menu{
    
    if(input$menu=="tab_results"){
      hide("panel_format")
      hide("panel_upload")
      show("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    } else if(input$menu == "subtab_upload"){
      hide("panel_format")
      show("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    } else if(input$menu == "subtab_format"){
      show("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    } else if(input$menu == "subtab_psi"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      show("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    
    } else if(input$menu == "subtab_sirs"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      show("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    
    } else if(input$menu == "subtab_quicksofa"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      show("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    
    } else if(input$menu == "subtab_halm"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      show("panel_halm")
      hide("panel_scap")
      hide("panel_smartcop")
    
    } else if(input$menu == "subtab_scap"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      show("panel_scap")
      hide("panel_smartcop")
    
    } else if(input$menu == "subtab_smartcop"){
      hide("panel_format")
      hide("panel_upload")
      hide("panel_results")
      hide("panel_psi")
      hide("panel_sirs")
      hide("panel_quicksofa")
      hide("panel_halm")
      hide("panel_scap")
      show("panel_smartcop")
    }
  }once = FALSE)
  
  # PROCESSES ----
  
  # Reactive Data Upload ----
  
  observeEvent(input$input.data{
    req(input$input.data)
    tryCatch(
      {
        # read the input data
        values$dat <- fread(input$input.data$datapath
                            sep = input$sep.data
                            quote = input$quote.data)
        
        # save the input data in the reactive value 
        #  <- input.data
        
        # return the input data
        # input.data
        
      } error = function(e) {
        stop(safeError(e))
      }
    )
  })
  
  # execute reactive reading of upload dataa
  output$data.preview.table <- DT::renderDataTable({values$dat})
  
  # Update Column Selection after Upload ----
  
  observeEvent(input$input.data{
    
    # get the data 
    dat <- values$dat

    # collect all inputIDs
    inputid.psi <- c("psi_id", "psi_age" "psi_sex" "psi_confusion" 
                     "psi_pulse" "psi_resprate" "psi_bpsys"
                     "psi_tempmin" "psi_tempmax" "psi_tumor" 
                     "psi_heart" "psi_cerebo" "psi_renal"
                     "psi_liver" "psi_nursehome" "psi_ph"
                     "psi_bun" "psi_snat" "psi_gluc" 
                     "psi_haem" "psi_apo2" "psi_pleu")
    
    # join ids
    my.inputid <- c(inputid.psi)
        
    # update all ids
    for(i in my.inputid){
      updateSelectInput(session ichoices = c(NULLnames(dat)))
    }
    
  })
  
  # Calculate Score ----
  
  observeEvent(input$run.calculate{
    
    # render output text in case not all colums are provided
    output$result.preview <- renderText( # TODO: link this to a datatable ----
      validate(
        need(
          expr = values$dat
          message = "No input data provided! Please upload your data!")
      )
    )
    
    # get the menu 
    menu.select <- input$menu
    message(menu.select)
    
    # data
    dat <- isolate(values$dat)
    message(paste(dim(dat)collapse = " "))
    
    # PSI ----
    if(menu.select=="subtab_psi"){
      
      # silent check for all inputs
      req({
        input$psi_age 
        input$psi_sex 
        input$psi_confusion 
        input$psi_pulse 
        input$psi_resprate 
        input$psi_bpsys
        input$psi_tempmin
        input$psi_tempmax
        input$psi_tumor 
        input$psi_heart 
        input$psi_cerebo 
        input$psi_renal
        input$psi_liver
        input$psi_nursehome 
        input$psi_ph
        input$psi_bun
        input$psi_snat
        input$psi_gluc 
        input$psi_haem 
        input$psi_apo2 
        input$psi_pleu
      })
      
      psi_simple(
        age = dat[,.SD,.SDcols=],
        gender = dat[,.SD,.SDcols=],
        verwirrt = dat[,.SD,.SDcols=],
        hfrq.max = dat[,.SD,.SDcols=],
        afrq.max = dat[,.SD,.SDcols=],
        sysbp.min = dat[,.SD,.SDcols=],
        temp.min = dat[,.SD,.SDcols=],
        temp.max = dat[,.SD,.SDcols=],
        tumor = dat[,.SD,.SDcols=],
        herz = dat[,.SD,.SDcols=],
        cerebro = dat[,.SD,.SDcols=],
        renal = dat[,.SD,.SDcols=],
        liver = dat[,.SD,.SDcols=],
        nurse.home = dat[,.SD,.SDcols=],
        art.ph.min = dat[,.SD,.SDcols=],
        bun = dat[,.SD,.SDcols=],
        snat = dat[,.SD,.SDcols=],
        gluk = dat[,.SD,.SDcols=],
        haemkrt = dat[,.SD,.SDcols=],
        apo2.min = dat[,.SD,.SDcols=],
        pleu_erg = dat[,.SD,.SDcols=])
      
      # SIRS ----
    } else if (menu.select=="subtab_sirs"){
      
      # quickSOFA ----
    } else if (menu.select=="subtab_quicksofa"){
      
      # Halm ----
    } else if (menu.select=="subtab_halm"){
      
      # SCAP ----
    } else if (menu.select=="subtab_scap"){
      
      # smartCOP ----
    } else if (menu.select=="subtab_smartcop"){
      
    }
    
    
    # output$calculated.score <-  "PLACEHOLDER"
    
    
    
  })
  
}