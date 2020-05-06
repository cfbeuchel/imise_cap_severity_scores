server <- function(input, output, session) {
  
  # Setup ----
  
  # for storage
  values <- reactiveValues()
  
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
  })
  
  # close startup message when pressing the button
  observeEvent(input$intro,{
    removeModal()
  })
  
  # UI - Calculate Score Button ----
  
  # watch for the calculation button and run the specific analysis
  observeEvent(
    input$run.calculate,{
      showModal(
        modalDialog(
          div(
            style="text-align: center;padding: 20px 20px",
            p("The selected score has been calculated! Please see the 'Results'-panel.",)
          ),
          easyClose = TRUE,
          footer = tagList(
            actionButton(
              inputId = "gotoresults",
              label = "Go To Results", 
              icon = icon("poll")
            )
          )
        ))
    })
  
  # After showing Info Button -> Go to results Panel
  observeEvent(input$gotoresults,{
    removeModal()
    hide("panel_format")
    hide("panel_upload")
    show("panel_results")
  })
  
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
          title = "Data Upload",{
            
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
        height = 320,
        tabPanel(
          title = "Data Preview"
        )
      )
    )
  })
  
  # Format Panel ----
  
  # Data Format Box - Info about necessary data format
  output$box_formatdescription <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_formatdescription",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Format"
        )
      )
    )
  })
  
  # Format example Box
  output$box_formatexample <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_formatexample",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Example"
        )
      )
    )
  })
  
  # Menu Input Panel ----
  
  # Show the different Score panels when selecting a submenu
  
  # ...
  
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
          title = "Downloads"
        )
      )
    )
  })
  
  # Data preview Box
  output$box_resultsview <- renderUI({
    div(
      style="position: relative; backgroundColor: #ecf0f5",
      tabBox(
        id = "box_resultsview",
        width = NULL,
        height = 320,
        tabPanel(
          title = "Results"
        )
      )
    )
  })
  
  # Dynamic Rendering ----
  
  # Init - show the upload panel by default
  observeEvent("", {
    show("panel_upload")
    # hide("panel_scoreselect")
    hide("panel_results")
  },once = TRUE)

  # UI based on menu selection
  observeEvent(input$menu,{
    
    if(input$menu=="tab_results"){
      hide("panel_format")
      hide("panel_upload")
      show("panel_results")
    } else if(input$menu %in% c("subtab_upload")){
      hide("panel_format")
      show("panel_upload")
      hide("panel_results")
    } else if(input$menu %in% c("subtab_format","subtab_upload")){
      show("panel_format")
      hide("panel_upload")
      hide("panel_results")
    }
    
    
  },once = FALSE)
  
}