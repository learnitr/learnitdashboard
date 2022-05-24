#' right_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_right_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("Course"),
    uiOutput(ns("sdd_app_selector")),
    uiOutput(ns("sdd_login_selector")),
    h3("Time"),
    checkboxInput(ns("is_dates"), h4("Select dates", style = "margin : 0px;")),
    uiOutput(ns("sdd_dates_selectors"))
  )
}
    
#' right_sidebar Server Functions
#'
#' @noRd 
mod_right_sidebar_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Global Vars / Connecting to DB ------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to them
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

# Display Selectors ---------------------------------------------------------------

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
    
    # Display // Login selector
    output$sdd_login_selector <- renderUI({
      # If there was no error while getting the logins
      if (!is.null(logins)) {
        tagList(
          tags$h3("Login :"),
          # Creation of selector with choices "All" and the logins
          selectInput(ns("sdd_selected_login"), NULL, choices = c("All", logins))
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
            # First selector of time
            column(width = 3,
                   tags$h3(""),
                   tags$br(),
                   tags$br(),
                   timeInput(ns("sdd_selected_time1"), NULL, value = input$sdd_selected_time1, seconds = FALSE)
            ),
            # Second selector of date
            column(width = 3,
                   tags$h3("To :"),
                   dateInput(ns("sdd_selected_date2"), NULL, value = input$sdd_selected_date2)
            ),
            # Second selector of time
            column(width = 3,
                   tags$h3(""),
                   tags$br(),
                   tags$br(),
                   timeInput(ns("sdd_selected_time2"), NULL, value = input$sdd_selected_time2, seconds = FALSE)
            ),
          )
        )
      } else {
        # To create the inputs but without making them available to user (for reactivity)
        tagList(
          shinyjs::hidden(dateInput(ns("sdd_selected_date1"), NULL, value = input$sdd_selected_date1)),
          shinyjs::hidden(timeInput(ns("sdd_selected_time1"), NULL, value = input$sdd_selected_time1)),
          shinyjs::hidden(dateInput(ns("sdd_selected_date2"), NULL, value = input$sdd_selected_date2)),
          shinyjs::hidden(timeInput(ns("sdd_selected_time2"), NULL, value = input$sdd_selected_time2)),
        )
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_right_sidebar_ui("right_sidebar_1")
    
## To be copied in the server
# mod_right_sidebar_server("right_sidebar_1")
