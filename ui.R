# initial function and library calls
source(file = "functions/app_init.R")

ui <- dashboardPage(
  
  # Dashboard Options ----
  title = "CAP Severity Scores",
  skin = "red",
  
  # Dashboard Parts ----
  # I used this cool looking app as orientation for the layout: https://shiny.rstudio.com/gallery/hospital-data-antimicrobial.html
  
  # ++++ ----
  
  # Dashboard Header ----
  dashboardHeader(
    title = strong("CAP Severity Scores"),
    
    # Top-Right ? Help Window ----
    dropdownMenu(
      type = "notifications", 
      headerText = strong("Help"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      
      # Quick workflow guide
      notificationItem(
        text = tags$div("See the 'Format' tab before",
                        tags$br(),
                        "uploading your data.",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("search")
      ),
      notificationItem(
        text = tags$div("Select data to upload.",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("table")
      ),
      notificationItem(
        text = tags$div("Choose a CAP Score to compute.",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("check-circle")
      ),
      notificationItem(
        text = tags$div("Select required data columns.",
                        style = "display: inline-block; vertical-align: middle;"),
        icon = icon("hand-pointer")
      )
    ),
    
    # Top-Right link to source package ----
    
    tags$li(
      a(
        strong("About"),
        height = 40,
        href = "https://github.com/maciejrosolowski/progressdatenbankderivate",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
    
  ),
  
  # ++++ ----
  
  # Dashboard Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      id = "menu",
      menuItem(
        text = "Data",
        tabName = "tab_data",
        icon = icon("table",
                    class = NULL, 
                    lib = "font-awesome"),
        menuSubItem("Format",
                    tabName = "subtab_format"),
        menuSubItem("Upload",
                    selected = TRUE,
                    tabName = "subtab_upload"
        )
      ),
      menuItem(
        text = "Score",
        tabName = "tab_score",
        icon = icon("calculator",
                    class = NULL, 
                    lib = "font-awesome"),
        menuSubItem("Score1",tabName = "subtab_score1"),
        menuSubItem("Score2"),
        menuSubItem("Score3"),
        menuSubItem("Score4"),
        menuSubItem("Score5"),
        menuSubItem("Score6")
      ),
      menuItem(
        text = "Results",
        tabName = "tab_results",
        icon = icon("poll",
                    class = NULL, 
                    lib = "font-awesome")
      )
    )
  ),
  
  # ++++ ----
  
  # Dashboard Body ----
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "style.css")
    ),
    
    # Use shinyjs functionality like hiding/showing stuff
    useShinyjs(),
    
    # First Row: Start Button ----
    
    fluidRow(
      column(
        width = 12,
        actionButton(
          inputId = "run.calculate", 
          label = strong("Calculate Score"), 
          icon = icon("play-circle"), 
          style='padding:20px; font-size:140%')
      )
    ),
    
    # Format Panel ----
    
    div(style = "padding: 20px 20px;",
        id = "panel_format",
        fluidRow(
          column(
            width=3,
            uiOutput("box_formatdescription")
          ),
          column(
            width=9,
            uiOutput("box_formatexample")
          )
        )
    ),
    
    # Upload Panel ----
    
    div(style = "padding: 20px 20px;",
        id = "panel_upload",
        fluidRow(
          column(
            width=3,
            uiOutput("box_upload")
          ),
          column(
            width=9,
            uiOutput("box_preview")
          )
        )
    ),
    
    # Score 1 Panel ----
    # Score X Panel ----
    
    # Results Panel ----
    
    div(style = "padding: 20px 20px;",
        id = "panel_results",
        fluidRow(
          column(
            width=3,
            uiOutput("box_resultsdownload")
          ),
          column(
            width=9,
            uiOutput("box_resultsview")
          )
        )
    )
    
  )
)