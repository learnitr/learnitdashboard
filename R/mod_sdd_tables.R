#' sdd_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sdd_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      # Column to select the columns to show
      column( width = 3,
        uiOutput(ns("dt_cols_selector")),
      ),
      
      # Column to select the login of the request
      column( width = 3,
        # Selector of login
        uiOutput(ns("sdd_login_selector")),
      ),
    ),
    
    fluidRow(
      
      # Column to select the app of the request
      column( width = 12,
        # Selector of app
        uiOutput(ns("sdd_app_selector")),
        
        tabsetPanel(id = ns("activetab"),
          # First tab
          tabPanel("H5P",
            tags$br(),
            DTOutput(ns("sdd_h5p_dt")),
          ),
          # Second tab
          tabPanel("Learnr",
            tags$br(),
            DTOutput(ns("sdd_learnr_dt")),
          ),
          # Third tab
          tabPanel("Shiny",
            tags$br(),
            DTOutput(ns("sdd_shiny_dt")),
          ),
        ),
      ),
    ),
    
  )
}
    
#' sdd_tables Server Functions
#'
#' @noRd 
mod_sdd_tables_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
# Global Vars -------------------------------------------------------------
    
    # URL pour accéder à la base de données
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # Pour se connecter
    sdd_h5p <- try(mongolite::mongo("h5p", url = sdd_url), silent = TRUE)
    sdd_learnr <- try(mongolite::mongo("learnr", url = sdd_url), silent = TRUE)
    sdd_shiny <- try(mongolite::mongo("shiny", url = sdd_url), silent = TRUE)
    
    # === H5P Variables ===
    # Variable : H5P
    # Initially getting all of 1000 last entries
    h5p_init <- try({message("requete h5p");sdd_h5p$find('{}', limit = 1000)}, silent = TRUE)
    h5p <- reactiveVal()
    # If it succeeded : it's defined in the main variable
    if (!inherits(h5p_init, "try-error")) {
      h5p(h5p_init)
    }
    
    # === Learnr Variables ===
    # Variable : Learnr
    # Initially getting all of 1000 last entries
    learnr_init <- try({message("requete learnr");sdd_learnr$find('{}', limit = 1000)}, silent = TRUE)
    learnr <- reactiveVal()
    # If it succeeded : it's defined in the main variable
    if (!inherits(learnr_init, "try-error")) {
      learnr(learnr_init)
    }
    
    # === Shiny Variables ===
    # Variable : Shiny
    # Initially getting all of 1000 last entries
    shiny_init <- try({message("requete shiny");sdd_shiny$find('{}', limit = 1000)}, silent = TRUE)
    shiny <- reactiveVal()
    # If it succeeded : it's defined in the main variable
    if (!inherits(shiny_init, "try-error")) {
      shiny(shiny_init)
    }
    
    # === SDD Variables ===
    # Variable : Users
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    # Variable : Logins
    logins <- try(sort(sdd_users$distinct("user_login")), silent = TRUE)
    # Disconnecting from users table
    try(sdd_users$disconnect(), silent = TRUE)
    # If logins occurred an error, it becomes NULL
    if (inherits(logins, "try-error")) {
      logins <- NULL
    }

# Reactive Values ---------------------------------------------------------

    # Variable : Definition of the request, "All" or only one selected login
    request <- eventReactive({
      input$sdd_selected_login
      input$sdd_selected_app
      }, {
      
      # Is there a login request ?
      login_request <- !is.null(input$sdd_selected_login) && input$sdd_selected_login != "All"
      # Is there an app request ?
      app_request <- !is.null(input$sdd_selected_app) && input$sdd_selected_app != "All"
      
      # Definition of the request
      # Request of login and app
      if (login_request && app_request) {
        return (paste0( r"( {"login" : ")", input$sdd_selected_login, r"(" , "app" : ")", input$sdd_selected_app, r"(" } )"))
      # Request of login
      } else if (login_request) {
        return(paste0( r"( {"login" : ")", input$sdd_selected_login, r"("} )"))
      # Request of app
      } else if (app_request) {
        return(paste0( r"( {"app" : ")", input$sdd_selected_app, r"("} )"))
      # No special request
      } else {
        return("{}")
      }
    })
    
    # Defining tables depending on the request
    observeEvent(request(), {
      {message("requete h5p");h5p(try(sdd_h5p$find(request(), limit = 1000), silent = TRUE))}
      {message("requete learnr");learnr(try(sdd_learnr$find(request(), limit = 1000), silent = TRUE))}
      {message("requete shiny");shiny(try(sdd_shiny$find(request(), limit = 1000), silent = TRUE))}
    })
    
# DT Displays -------------------------------------------------------------
    
    # Display // DT cols selector
    output$dt_cols_selector <- renderUI({
      # If there was no error while loading the table
      if (!inherits(h5p_init, "try-error")) {
        tagList(
          tags$h3("Columns :"),
          # Creation of the selector of cols to show, because the page is too small for everything
          selectInput(ns("dt_selected_cols"), NULL, choices = c("All",names(h5p_init)), multiple = TRUE, selected = "All")
        )
      } else { NULL }
    })
    
    # Display // Login selector
    output$sdd_login_selector <- renderUI({
      # If there was no error while getting the logins
      if (!is.null(logins)) {
        tagList(
          tags$h3("Login :"),
          # Creation of selector with choices "All" and the logins
          selectInput(ns("sdd_selected_login"), NULL, choices = c("All", logins), selected = "All")
        )
      } else { NULL }
    })
    
    # Display // App selector
    output$sdd_app_selector <- renderUI({
      req(input$activetab)
      
      # Getting the right table
      table <- switch (input$activetab,
        "H5P" = sdd_h5p,
        "Learnr" = sdd_learnr,
        "Shiny" = sdd_shiny
      )
      
      # Getting table's apps
      table_apps <- try(sort(table$distinct("app")), silent = TRUE)
      
      # Displaying the selector if table_apps didn't occur error
      if (!inherits(table_apps, "try-error")) {
        tagList(
          tags$h3("Application :"),
          selectInput(ns("sdd_selected_app"), NULL, choices = c("All", table_apps), selected = "All")
        )
      }
    })
    
    # Display // H5P datatable
    output$sdd_h5p_dt <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(h5p(), "try-error") && length(h5p()) > 0) {
        # Displaying everything or only selected cols
        if ("All" %in% req(input$dt_selected_cols)) {
          h5p()
        } else {
          h5p()[req(input$dt_selected_cols)]
        }
      } else {
        NULL
      }
    },
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5,10,25,50)
    )
    )
    
    # Display // Learnr datatable
    output$sdd_learnr_dt <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(learnr(), "try-error") && length(learnr()) > 0) {
        # Displaying everything or only selected cols
        if ("All" %in% req(input$dt_selected_cols)) {
          learnr()
        } else {
          learnr()[req(input$dt_selected_cols)]
        }
      } else {
        NULL
      }
    },
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5,10,25,50)
    )
    )
    
    # Display // Shiny datatable
    output$sdd_shiny_dt <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(shiny(), "try-error") && length(shiny()) > 0) {
        # Displaying everything or only selected cols
        if ("All" %in% req(input$dt_selected_cols)) {
          shiny()
        } else {
          shiny()[req(input$dt_selected_cols)]
        }
      } else {
        NULL
      }
    },
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 5,
      lengthMenu = c(5,10,25,50)
    )
    )
    
  })
}
    
## To be copied in the UI
# mod_sdd_tables_ui("sdd_tables_1")
    
## To be copied in the server
# mod_sdd_tables_server("sdd_tables_1")
