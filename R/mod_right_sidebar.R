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
      checkboxInput(ns("is_dates"), h4("Select Dates", style = "margin : 0px;")),
      uiOutput(ns("ui_dates_selectors")),
      # Selector of news_time
      uiOutput(ns("ui_news_time_selector"))
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
    sdd_events <- try(mongolite::mongo("events", url = sdd_url), silent = TRUE)
    # sdd_h5p <- try(mongolite::mongo("h5p", url = sdd_url), silent = TRUE)
    # sdd_learnr <- try(mongolite::mongo("learnr", url = sdd_url), silent = TRUE)
    # sdd_shiny <- try(mongolite::mongo("shiny", url = sdd_url), silent = TRUE)
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    sdd_users2 <- try(mongolite::mongo("users2", url = sdd_url), silent = TRUE)
    sdd_apps <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)
    sdd_planning <- try(mongolite::mongo("planning", url = sdd_url), silent = TRUE)
    
    # === Events Table ===
    # Variable : Events
    events <- reactiveVal()
    
    # === H5P Table ===
    # Variable : H5P
    h5p <- reactiveVal()
    
    # === Learnr Table ===
    # Variable : Learnr
    learnr <- reactiveVal()
    
    # === Shiny Table ===
    # Variable : Shiny
    shiny <- reactiveVal()
    
    # === Apps Table ===
    apps <- reactiveVal()
    
    # === Planning Table ===
    planning <- reactiveVal()
    
    # Variable : Courses
    courses <- try(sort(sdd_apps$distinct("icourse")), silent = TRUE)

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
    
    # Display // Module selector
    output$ui_module_selector <- renderUI({
      
      # Selecting the modules : If not All : modules from the table apps for selected course
      if (req(input$selected_course) != "All") {
        modules <- try(sort(sdd_apps$distinct("module", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      } else {
      # Else : modules from the apps table for all courses
        modules <- try(sort(sdd_apps$distinct("module")), silent = TRUE)
      }
      
      # modules to NULL if error while getting them
      if(inherits(modules, "try-error")) {
        modules <- NULL
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
      
      # Getting apps
      # If module and course selected
      if (req(input$selected_module) != "All" && req(input$selected_course) != "All") {
        sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" , "module" : "<<input$selected_module>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If only module selected
      } else if (req(input$selected_module) != "All" && req(input$selected_course) == "All") {
        sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "module" : "<<input$selected_module>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If only course selected
      } else if (req(input$selected_module) == "All" && req(input$selected_course) != "All") {
        sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
      # If nothing selected
      } else {
        sel_apps <- try(sort(sdd_apps$distinct("app")), silent = TRUE)
      }
      
      # Displaying the selector if apps didn't occur error
      if (!inherits(sel_apps, "try-error")) {
        tagList(
          tags$h3("Application :"),
          selectInput(ns("selected_app"), NULL, choices = c("All", sel_apps), selected = "All")
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
          logins <- try(sort(sdd_users2$distinct("login", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" , "enrolled" : true }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        # If not only the enrolled
        } else {
          logins <- try(sort(sdd_users2$distinct("login", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        }
      # Getting the logins from all courses
      } else {
        # If only the enrolled
        if (input$only_enrolled == TRUE) {
          logins <- try(sort(sdd_users2$distinct("login", query = r"--[{"enrolled" : true}]--")), silent = TRUE)
        # If not only the enrolled
        } else {
          logins <- try(sort(sdd_users2$distinct("login")), silent = TRUE)
        }
      }
      
      # If no errors to get the logins : Display the selector
      if (!inherits(logins, "try-error") && length(logins > 0)) {
        tagList(
          # Creation of selector with choices "All" and the logins of course or all
          selectInput(ns("selected_login"), NULL, choices = c("All", logins))
        )
      # Create an invisible selector with value NULL to get nothing from the request
      } else { 
        selectInput(ns("selected_login"), NULL, choices = "NULL")
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
    
    # Display // News Time selector
    output$ui_news_time_selector <- renderUI({
      
      if (!inherits(sdd_users, "try-error")) {
        # Setting the date to 7 days before the actual date
        time <- Sys.time()
        lubridate::day(time) <- lubridate::day(time) - 7
        
        # Displaying the selector
        tagList(
          hr(),
          h3("See News From :"),
          dateInput(ns("selected_news_time"), NULL, value = time)
          # timeInput()
        )
      } else { NULL }
    })
    
# Main Request ---------------------------------------------------------
    
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
      
      # Creation of the request part for course if request there is
      if (is_request(input$selected_course)) {
        request_vector <- c(request_vector, "icourse" = glue::glue(r"--["icourse" : "<<input$selected_course>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for module if request there is
      if (is_request(input$selected_module) && !is_request(input$selected_app)) {
        request_vector <- c(request_vector, "module" = glue::glue(r"--["module" : "<<input$selected_module>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for app if request there is
      if (is_request(input$selected_app)) {
        request_vector <- c(request_vector, "app" = glue::glue(r"--["app" : "<<input$selected_app>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for user if request there is
      if (is_request(input$selected_login)) {
        # Preparation of the request to get user from login
        users2_request <- glue::glue(r"--[{"login" : "<<input$selected_login>>"}]--", .open = "<<", .close = ">>")
        # Get the user from the selected login inside of the users2 table
        user <- try(unique(sdd_users2$find(users2_request, fields = '{"user" : true}')$user), silent = TRUE)
        request_vector <- c(request_vector, "user" = glue::glue(r"--["user" : "<<user>>"]--", .open = "<<", .close = ">>"))
      }
      
      # Creation of the request part for dates if request there is
      if (input$is_dates == TRUE) {
        # Preparation of the dates
        date_from <- as.POSIXct(paste0(input$selected_date1, " ", as.character(strftime(input$selected_time1, "%R"))), tz = "UTC")
        date_from <- format(date_from, "%Y-%m-%dT%H:%M:%SZ")
        date_to <- as.POSIXct(paste0(input$selected_date2, " ", as.character(strftime(input$selected_time2, "%R"))), tz = "UTC")
        date_to <- format(date_to, "%Y-%m-%dT%H:%M:%SZ")
        # Preparation of the request
        request_vector <- c(request_vector, "dates" = glue::glue(r"--["date" : { "$gte" : {"$date" : "<<date_from>>"} , "$lte" : {"$date" : "<<date_to>>"} }]--", .open = "<<", .close = ">>"))
        request_vector <- c(request_vector, "start_end_apps" = glue::glue(r"--["start" : { "$gte" : {"$date" : "<<date_from>>"} } , "end" : { "$lte" : {"$date" : "<<date_to>>"} }]--", .open = "<<", .close = ">>"))
        request_vector <- c(request_vector, "start_end" = glue::glue(r"--["start" : { "$gte" : "<<paste0(input$selected_date1, " ", strftime(input$selected_time1, "%H:%M"))>>" } , "end" : { "$lte" : "<<paste0(input$selected_date2, " ", strftime(input$selected_time2, "%H:%M"))>>" }]--", .open = "<<", .close = ">>"))
      }
      
      # Definition of the request
      # build_request <- r"--[{<<paste(request_vector, collapse = " , ")>>}]--"
      
      # Send the request after evaluating it
      # request(glue::glue(build_request, .open = "<<", .close = ">>"))
      
      # If the vector is not null, return the vector, if it is, return "empty" to make empty request 
      if (!is.null(request_vector)) {
        request(request_vector)
      } else {
        request("empty")
      }
    })
    
    # Defining of main tables depending on the request
    observeEvent(request(), {
      # Only args used for the events tables
      events_args <- c("icourse", "module", "app", "user", "dates", "type")
      apps_args <- c("icourse", "module", "app", "start_end_apps")
      planning_args <- c("icourse", "start_end")
      # --- Preparing the request for the events tables
      # 1 : Events
      events_request <- prepare_request(request(), events_args)
      # 2 : H5P
      h5p_request <- prepare_request(request(), events_args, type = "h5p")
      # 3 : Learnr
      learnr_request <- prepare_request(request(), events_args, type = "learnr")
      # 4 : Shiny
      shiny_request <- prepare_request(request(), events_args, type = "shiny")
      # 5 : Apps
      apps_request <- prepare_request(request(), apps_args)
      # 6 : Planning
      planning_request <- prepare_request(request(), planning_args)
      
      print(events_request)
      print(apps_request)
      
      # --- Preparing the logins from the users
      users2 <- try(unique(sdd_users2$find('{}', fields = '{"user" : true, "login" : true, "_id" : false}')), silent = TRUE)
      users_login <- users2$login
      names(users_login) <- users2$user
      
      # --- Requesting to databases
      {message("requete events");events(try(sdd_events$find(events_request, limit = 10000), silent = TRUE))}
      
      # Preparation of the h5p table with good logins instead of users
      h5p_table <- {message("requete h5p"); try(sdd_events$find(h5p_request, limit = 1000), silent = TRUE)}
      h5p_table$user <- as.character(users_login[h5p_table$user])
      h5p(h5p_table)
      # Preparation of the learnr table with good logins instead of users
      learnr_table <- {message("requete learnr"); try(sdd_events$find(learnr_request, limit = 1000), silent = TRUE)}
      learnr_table$user <- as.character(users_login[learnr_table$user])
      learnr(learnr_table)
      # Preparation of the shiny table with good logins instead of users
      shiny_table <- {message("requete shiny"); try(sdd_events$find(shiny_request, limit = 1000), silent = TRUE)}
      shiny_table$user <- as.character(users_login[shiny_table$user])
      shiny(shiny_table)
      
      {message("requete apps");apps(try(sdd_apps$find(apps_request), silent = TRUE))}
      {message("requete planning");planning(try(sdd_planning$find(planning_request), silent = TRUE))}
    })

# News Request ------------------------------------------------------------

    # Variable : Request for news
    news_request <- reactive({
      request_vector <- c()
      news_time <- format(as.POSIXct(req(input$selected_news_time), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
      request_vector <- c(request_vector, "news_date" = glue::glue(r"--["date" : { "$gte" : {"$date" : "<<news_time>>"} }]--", .open = "<<", .close = ">>"))
    })
    
    # Variable : h5p table with request for news
    h5p_news <- reactive({
      request <- prepare_request(news_request(), c("news_date", "type"), type = "h5p")
      news <- try(sdd_events$count(request), silent = TRUE)
      attr(news, "apps") <- try(sdd_events$distinct("app", query = request), silent = TRUE)
      attr(news, "courses") <- try(sdd_events$distinct("course", query = request), silent = TRUE)
      return(news)
    })
    # Variable : learnr table with request for news
    learnr_news <- reactive({
      request <- prepare_request(news_request(), c("news_date", "type"), type = "learnr")
      news <- try(sdd_events$count(request), silent = TRUE)
      attr(news, "apps") <- try(sdd_events$distinct("app", query = request), silent = TRUE)
      attr(news, "courses") <- try(sdd_events$distinct("course", query = request), silent = TRUE)
      return(news)
    })
    # Variable : shiny table with request for news
    shiny_news <- reactive({
      request <- prepare_request(news_request(), c("news_date", "type"), type = "shiny")
      news <- try(sdd_events$count(request), silent = TRUE)
      attr(news, "apps") <- try(sdd_events$distinct("app", query = request), silent = TRUE)
      attr(news, "courses") <- try(sdd_events$distinct("course", query = request), silent = TRUE)
      return(news)
    })

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    right_sidebar_vars <- reactiveValues(
      selected_login = NULL,
      selected_course = NULL,
      events = NULL,
      h5p = NULL,
      learnr = NULL,
      shiny = NULL,
      apps = NULL,
      planning = NULL,
      h5p_news = NULL,
      learnr_news = NULL,
      shiny_news = NULL,
      selected_news_time = NULL,
      selected_module = NULL,
      selected_app = NULL,
    )
    
    # Updating the vars
    observe({
      right_sidebar_vars$selected_login <- input$selected_login
      right_sidebar_vars$selected_course <- input$selected_course
      right_sidebar_vars$events <- events()
      right_sidebar_vars$h5p <- h5p()
      right_sidebar_vars$learnr <- learnr()
      right_sidebar_vars$shiny <- shiny()
      right_sidebar_vars$apps <- apps()
      right_sidebar_vars$planning <- planning()
      right_sidebar_vars$h5p_news <- h5p_news()
      right_sidebar_vars$learnr_news <- learnr_news()
      right_sidebar_vars$shiny_news <- shiny_news()
      right_sidebar_vars$selected_news_time <- input$selected_news_time
      right_sidebar_vars$selected_module <- input$selected_module
      right_sidebar_vars$selected_app <- input$selected_app
    })
    
    return(right_sidebar_vars)
    
  })
}
    
## To be copied in the UI
# mod_right_sidebar_ui("right_sidebar_1")
    
## To be copied in the server
# mod_right_sidebar_server("right_sidebar_1")
