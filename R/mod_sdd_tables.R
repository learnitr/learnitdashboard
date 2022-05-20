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
    
    # 1st Row
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
    
    # 2nd Row
    fluidRow(
      column( width = 3,
        # Activator of selecting dates
        checkboxInput(ns("is_dates"), h4("Select dates", style = "margin : 0px;")),
      )
    ),
    
    # 3rd Row
    uiOutput(ns("sdd_dates_selectors")),
    
    # 4th Row
    fluidRow(
      
      # Column to select the app of the request
      column( width = 12,
        # Selector of app
        uiOutput(ns("sdd_app_selector")),
        
        tabsetPanel(id = ns("activetab"),
          # First tab : H5P
          tabPanel("H5P",
            tags$br(),
            DTOutput(ns("sdd_h5p_dt")),
          ),
          # Second tab : Learnr
          tabPanel("Learnr",
            tags$br(),
            DTOutput(ns("sdd_learnr_dt")),
          ),
          # Third tab : Shiny
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
    h5p <- reactiveVal()
    
    # === Learnr Variables ===
    # Variable : Learnr
    learnr <- reactiveVal()
    
    # === Shiny Variables ===
    # Variable : Shiny
    shiny <- reactiveVal()
    
    # === SDD Variables ===
    # Variable : Users
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    # Variable : Logins
    # logins <- try(sort(sdd_users$distinct("user_login")), silent = TRUE)
    logins <- try(sort(unique(c(sdd_users$distinct("user_login"), sdd_h5p$distinct("login")))), silent = TRUE)
    # Disconnecting from users table
    try(sdd_users$disconnect(), silent = TRUE)
    # If logins occurred an error, it becomes NULL
    if (inherits(logins, "try-error") || length(logins) == 0) {
      logins <- NULL
    }


# Selectors ---------------------------------------------------------------

    
    # Display // DT cols selector
    output$dt_cols_selector <- renderUI({
      req(input$activetab)
      
      # Getting the right table depending on the tab
      table <- switch (input$activetab,
                       "H5P" = h5p(),
                       "Learnr" = learnr(),
                       "Shiny" = shiny()
      )
      
      # If there was no error while loading the table
      if (!inherits(table, "try-error")) {
        tagList(
          tags$h3("Columns :"),
          # Creation of the selector of cols to show, because the page is too small for everything
          selectInput(ns("dt_selected_cols"), NULL, choices = c("All",names(table)), multiple = TRUE, selected = "All")
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
    
    # Display // Dates selectors
    output$sdd_dates_selectors <- renderUI({
      if (input$is_dates == TRUE) {
        tagList(
          fluidRow(
            # First selector of date
            column(width = 3,
                   tags$h3("From :"),
                   dateInput(ns("sdd_selected_date1"), NULL, value = input$sdd_selected_date1)
            ),
            # Second selector of date
            column(width = 3,
                   tags$h3("To :"),
                   dateInput(ns("sdd_selected_date2"), NULL, value = input$sdd_selected_date2)
            ),
          )
        )
      } else {
        tagList(
          shinyjs::hidden(dateInput(ns("sdd_selected_date1"), NULL, value = input$sdd_selected_date1)),
          shinyjs::hidden(dateInput(ns("sdd_selected_date2"), NULL, value = input$sdd_selected_date2)),
        )
      }
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
    
# Request ---------------------------------------------------------
    
    # Variable : Definition of the request, "All" or only one selected login
    request <- eventReactive({
      print(input$sdd_selected_login)
      print(input$sdd_selected_app)
      print(input$is_dates)
      print(input$sdd_selected_date1)
      print(input$sdd_selected_date2)
      }, {
      # Is there a login request ?
      login_request <- !is.null(input$sdd_selected_login) && input$sdd_selected_login != "All"
      if (login_request) {
        login_querry <- glue::glue(r"("login" : "<<input$sdd_selected_login>>")", .open = "<<", .close = ">>")
      }
      # Is there an app request ?
      app_request <- !is.null(input$sdd_selected_app) && input$sdd_selected_app != "All"
      if (app_request) {
        app_querry <- glue::glue(r"("app" : "<<input$sdd_selected_app>>")", .open = "<<", .close = ">>")
      }
      # Is there a date request ?
      date_request <- input$is_dates == TRUE
      if (date_request) {
        date_querry <- glue::glue(r"("date" : { "$gte" : "<<input$sdd_selected_date1>>" , "$lte" : "<<input$sdd_selected_date2>>" })", .open = "<<", .close = ">>")
      }
      
      # Definition of the request
      # Request of login and app
      if (login_request && app_request) {
        if (date_request) {
          request <- r"({<<login_querry>> , <<app_querry>> , <<date_querry>>})"
        } else {
          request <- r"({<<login_querry>> , <<app_querry>>})"
        }
      # Request of login
      } else if (login_request) {
        if (date_request) {
          request <- r"({<<login_querry>> , <<date_querry>>})"
        } else {
          request <- r"({<<login_querry>>})"
        }
      # Request of app
      } else if (app_request) {
        if (date_request) {
          request <- r"({<<app_querry>> , <<date_querry>>})"
        } else {
          request <- r"({<<app_querry>>})"
        }
      # No special request
      } else if (date_request) {
        request <- r"({<<date_querry>>})"
      } else {
        request <- "{}"
      }
      return(glue::glue(request, .open = "<<", .close = ">>"))
    })
    
    # Defining tables depending on the request
    observeEvent(request(), {
      print(request())
      {message("requete h5p");h5p(try(sdd_h5p$find(request(), limit = 1000), silent = TRUE))}
      {message("requete learnr");learnr(try(sdd_learnr$find(request(), limit = 1000), silent = TRUE))}
      {message("requete shiny");shiny(try(sdd_shiny$find(request(), limit = 1000), silent = TRUE))}
    })
    
# DT Displays -------------------------------------------------------------
    
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
      pageLength = 100,
      lengthMenu = c(50,100,200,500)
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
      pageLength = 100,
      lengthMenu = c(50,100,200,500)
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
      pageLength = 100,
      lengthMenu = c(50,100,200,500)
    )
    )
    

# Communication -----------------------------------------------------------

    sdd_tables_vars <- reactiveValues(
      logins = NULL,
    )
    
    observe({
      sdd_tables_vars$logins <- logins
    })
    
    return(logins)
    
  })
}
    
## To be copied in the UI
# mod_sdd_tables_ui("sdd_tables_1")
    
## To be copied in the server
# mod_sdd_tables_server("sdd_tables_1")
