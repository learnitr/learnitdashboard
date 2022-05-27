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
    column( width = 12,
      # Selector of course :
      uiOutput(ns("ui_course_selector")),
      # Selector of table :
      uiOutput(ns("ui_table_selector")),
      # Selector of module :
      uiOutput(ns("ui_module_selector")),
      # Selector of app :
      uiOutput(ns("ui_app_selector")),
      # Selector of enrolled :
      uiOutput(ns("ui_enrolled_selector")),
      # Selector of login :
      uiOutput(ns("ui_login_selector")),
      # Selectors of time range :
      tags$h3("Time :"),
      checkboxInput(ns("is_dates"), h4("Select dates", style = "margin : 0px;")),
      uiOutput(ns("ui_dates_selectors"))
    )
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
    all_logins <- try(sort(unique(c(sdd_users$distinct("user_login"), sdd_h5p$distinct("login")))), silent = TRUE)
    # Variable : Courses
    courses <- try(sort(sdd_users$distinct("icourse")), silent = TRUE)
    # Variable : Apps
    # h5p_apps <- try(sdd_h5p$distinct("app"), silent = TRUE)
    # learnr_apps <- try(sdd_learnr$distinct("app"), silent = TRUE)
    # shiny_apps <- try(sdd_shiny$distinct("app"), silent = TRUE)
    # all_modules <- try(sort(unique(substring(c(h5p_apps, learnr_apps, shiny_apps), 1, 3))), silent = TRUE)

    # If all_logins occurred an error or is empty, it becomes NULL
    if (inherits(all_logins, "try-error") || length(all_logins) == 0) {all_logins <- NULL}
    # If courses occured an error or is empty, it becomes NULL
    if (inherits(courses, "try-error") || length(courses) == 0) {courses <- NULL}

# Display Selectors ---------------------------------------------------------------

    # Display // Course selector
    output$ui_course_selector <- renderUI({
      if (!is.null(courses)) {
        tagList(
          tags$h3("Course :"),
          # Creation of selector with choices "All and the courses
          selectInput(ns("selected_course"), NULL, choices = c("All", courses))
        )
      } else { NULL }
    })
    
    # Display // Table selector
    output$ui_table_selector <- renderUI({
      tagList(
        tags$h3("Table :"),
        selectInput(ns("selected_table"), NULL, choices = c("H5P", "Learnr", "Shiny"), selected = "H5P")
      )
    })
    
    # Display // Module selector
    output$ui_module_selector <- renderUI({
      req(input$selected_table)
      
      # Getting the right table
      table <- switch (input$selected_table,
                       "H5P" = sdd_h5p,
                       "Learnr" = sdd_learnr,
                       "Shiny" = sdd_shiny
      )
      
      # Selecting the modules : If not All : modules from the selected table and course
      if (req(input$selected_course) != "All") {
        modules <- try(sort(unique(substring(table$distinct("app", query = glue::glue(r"--[{ "course" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>")), 1, 3))), silent = TRUE)
      } else {
      # Else : modules from the selcted table
        modules <- try(sort(unique(substring(table$distinct("app"), 1, 3))), silent = TRUE)
      }
      
      # Elements to display
      tagList(
        tags$h3("Module :"),
        # Creation of the module selector
        selectInput(ns("selected_module"), NULL, choices = c("All", modules))
      )
    })
    
    # Display // App selector
    output$ui_app_selector <- renderUI({
      req(input$selected_table)
      
      # Getting the right table
      table <- switch (input$selected_table,
                       "H5P" = sdd_h5p,
                       "Learnr" = sdd_learnr,
                       "Shiny" = sdd_shiny
      )
      
      # Getting table's apps
      # If module and course selected
      if (req(input$selected_module) != "All" && req(input$selected_course) != "All") {
        table_apps <- try(sort(table$distinct("app", query = glue::glue(r"--[{ "course" : "<<input$selected_course>>" , "app" : { "$regex" : "^<<input$selected_module>>" , "$options" : "" } }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If only module selected
      } else if (req(input$selected_module) != "All" && req(input$selected_course) == "All") {
        table_apps <- try(sort(table$distinct("app", query = glue::glue(r"--[{ "app" : { "$regex" : "^<<input$selected_module>>" , "$options" : "" } }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If only course selected
      } else if (req(input$selected_module) == "All" && req(input$selected_course) != "All") {
        table_apps <- try(sort(table$distinct("app", query = glue::glue(r"--[{ "course" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If nothing selected
      } else {
        table_apps <- try(sort(table$distinct("app")), silent = TRUE)
      }
      
      # Displaying the selector if table_apps didn't occur error
      if (!inherits(table_apps, "try-error")) {
        tagList(
          tags$h3("Application :"),
          selectInput(ns("selected_app"), NULL, choices = c("All", table_apps), selected = "All")
        )
      }
    })
    
    # Display // Enrolled selector
    output$ui_enrolled_selector <- renderUI({
      
      if (!inherits(sdd_users, "try-error")) {
      tagList(
        tags$h3("Login :"),
        checkboxInput(ns("only_enrolled"), "Only Enrolled", value = FALSE)
      )
      } else { NULL }
    })
    
    # Display // Login selector
    output$ui_login_selector <- renderUI({
      
      logins <- c()
      # Getting the logins from selected course
      if (req(input$selected_course) != "All") {
        # If only the enrolled
        if (input$only_enrolled == TRUE) {
          logins <- try(sort(sdd_users$distinct("user_login", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" , "enrolled" : "yes" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        # If not only the enrolled
        } else {
          logins <- try(sort(sdd_users$distinct("user_login", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        }
      # Getting the logins from all courses
      } else {
        # If only the enrolled
        if (input$only_enrolled == TRUE) {
          logins <- try(sort(sdd_users$distinct("user_login", query = r"--[{"enrolled" : "yes"}]--")), silent = TRUE)
        # If not only the enrolled
        } else {
          logins <- try(sort(unique(c(sdd_users$distinct("user_login"), sdd_h5p$distinct("login")))), silent = TRUE)
        }
      }
      print(length(logins))
      
      # If no errors to get the logins : Display the selector
      if (!inherits(logins, "try-error") && length(logins > 0)) {
        tagList(
          # Creation of selector with choices "All" and the logins of course or all
          selectInput(ns("selected_login"), NULL, choices = c("All", logins))
        )
      # Create an invisible selector with value NULL to get nothing from the request
      } else { 
        shinyjs::hidden(selectInput(ns("selected_login"), NULL, choices = "NULL"))
      }
      
    })
    
    # Display // Dates selectors
    output$ui_dates_selectors <- renderUI({
      if (input$is_dates == TRUE) {
        tagList(
          # First selector of date
          tags$h4("From :"),
          dateInput(ns("selected_date1"), NULL, value = input$selected_date1),
          # First selector of time
          timeInput(ns("selected_time1"), NULL, value = input$selected_time1, seconds = FALSE),
          # Second selector of date
          tags$h4("To :"),
          dateInput(ns("selected_date2"), NULL, value = input$selected_date2),
          # Second selector of time
          timeInput(ns("selected_time2"), NULL, value = input$selected_time2, seconds = FALSE),
        )
      } else {
        # To create the inputs but without making them available to user (for reactivity)
        tagList(
          shinyjs::hidden(dateInput(ns("selected_date1"), NULL, value = input$selected_date1)),
          shinyjs::hidden(timeInput(ns("selected_time1"), NULL, value = input$selected_time1)),
          shinyjs::hidden(dateInput(ns("selected_date2"), NULL, value = input$selected_date2)),
          shinyjs::hidden(timeInput(ns("selected_time2"), NULL, value = input$selected_time2)),
        )
      }
    })
    
# Request ---------------------------------------------------------
    
    # Variable : Request
    request <- reactiveVal()
    
    # Variable : Definition of the request depending on login, app and dates/times
    observeEvent({
      input$selected_course
      input$selected_login
      input$selected_module
      input$selected_app
      input$is_dates
      input$selected_date1
      input$selected_date2
      input$selected_time1
      input$selected_time2
    }, {
      # Creation of empty vector for the request
      request_vector <- c()
      
      # --- Is there a course request ?
      course_request <- !is.null(input$selected_course) && input$selected_course != "All"
      # --- Is there a module request  ?
      mod_request <- !is.null(input$selected_module) && input$selected_module != "All"
      # --- Is there an app request ?
      app_request <- !is.null(input$selected_app) && input$selected_app != "All"
      # --- Is there a login request ?
      login_request <- !is.null(input$selected_login) && input$selected_login != "All"
      # --- Is there a date request ?
      date_request <- input$is_dates == TRUE
      
      # Creation of the request part for course
      if (course_request) {
        request_vector <- append(request_vector, glue::glue(r"--["course" : "<<input$selected_course>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for module
      if (mod_request && !app_request) {
        request_vector <- append(request_vector, glue::glue(r"--["app" : { "$regex" : "<<input$selected_module>>", "$options" : "" }]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for app
      if (app_request) {
        request_vector <- append(request_vector, glue::glue(r"--["app" : "<<input$selected_app>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for login
      if (login_request) {
        request_vector <- append(request_vector, glue::glue(r"--["login" : "<<input$selected_login>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for dates
      if (date_request) {
        request_vector <- append(request_vector, glue::glue(r"--["date" : { "$gte" : "<<paste0(input$selected_date1, " ", strftime(input$selected_time1, "%H:%M"))>>" , "$lte" : "<<paste0(input$selected_date2, " ", strftime(input$selected_time2, "%H:%M"))>>" }]--", .open = "<<", .close = ">>"))
      }
      
      # Definition of the request
      build_request <- r"--[{<<paste(request_vector, collapse = " , ")>>}]--"
      
      # Send the request after evaluating it
      request(glue::glue(build_request, .open = "<<", .close = ">>"))
    })
    
    # Defining tables depending on the request
    observeEvent(request(), {
      print(request())
      {message("requete h5p");h5p(try(sdd_h5p$find(request(), limit = 1000), silent = TRUE))}
      {message("requete learnr");learnr(try(sdd_learnr$find(request(), limit = 1000), silent = TRUE))}
      {message("requete shiny");shiny(try(sdd_shiny$find(request(), limit = 1000), silent = TRUE))}
    })
    

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    right_sidebar_vars <- reactiveValues(
      selected_table = NULL,
      selected_login = NULL,
      selected_course = NULL,
      h5p = NULL,
      learnr = NULL,
      shiny = NULL,
    )
    
    # Updating the vars
    observe({
      right_sidebar_vars$selected_table <- input$selected_table
      right_sidebar_vars$selected_login <- input$selected_login
      right_sidebar_vars$selected_course <- input$selected_course
      right_sidebar_vars$h5p <- h5p()
      right_sidebar_vars$learnr <- learnr()
      right_sidebar_vars$shiny <- shiny()
    })
    
    return(right_sidebar_vars)
    
  })
}
    
## To be copied in the UI
# mod_right_sidebar_ui("right_sidebar_1")
    
## To be copied in the server
# mod_right_sidebar_server("right_sidebar_1")
