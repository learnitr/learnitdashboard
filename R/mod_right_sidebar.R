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
      # Selector of login :
      uiOutput(ns("ui_login_selector")),
      # Selector of enrolled :
      uiOutput(ns("ui_enrolled_selector")),
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

# Tables Reactive Vars ------------------------------------------
    
    # === Events Table ===
    # Variable : Events
    events <- reactiveVal()
    
    # === Courses Table ===
    courses <- reactiveVal()
    
    # === Modules Table ===
    modules <- reactiveVal()
    
    # === Apps Table ===
    apps <- reactiveVal()
    
    # === Planning Table ===
    planning <- reactiveVal()
    
    # === Users2 Table ===
    users2 <- reactiveVal()
    
    # Variable : Courses
    # courses <- try(sort(sdd_apps$distinct("icourse")), silent = TRUE)

    # If courses occured an error or is empty, it becomes NULL
    # if (inherits(courses, "try-error") || length(courses) == 0) {courses <- NULL}

# Display Selectors ---------------------------------------------------------------

    # Display // Course selector
    output$ui_course_selector <- renderUI({
      if (!inherits(sdd_courses, "try-error")) {
        courses_df <- unique(courses_init[,c("icourse", "ictitle")])
        sel_courses <- courses_df$icourse
        names(sel_courses) <- courses_df$ictitle
        tagList(
          tags$h3("Course :"),
          # Creation of selector with choices "All and the courses
          selectInput(ns("selected_course"), NULL, choices = c("All", sel_courses))
        )
      }
    })
    
    # Display // Module selector
    output$ui_module_selector <- renderUI({
      if (!inherits(sdd_modules, "try-error")) {
        
        req(input$selected_course)
        
        # Getting the users with the function from the selectors-
        sel_modules <- try(get_modules(input$selected_course, modules_init, acad_year), silent = TRUE)
        
        # modules to NULL if error while getting them
        # if(inherits(modules, "try-error")) {
        #   modules <- NULL
        # }
        
        # Elements to display
        tagList(
          tags$h3("Module :"),
          # Creation of the module selector
          selectInput(ns("selected_module"), NULL, choices = c("All", sel_modules))
        )
      }
    })
    
    # Display // App selector
    output$ui_app_selector <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        req(input$selected_course)
        req(input$selected_module)
        
        # Getting apps
        # If module and course selected
        if (input$selected_module != "All" && input$selected_course != "All") {
          sel_apps <- unique(apps_init[apps_init$icourse == input$selected_course & apps_init$module == input$selected_module, "app"])
          # sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" , "module" : "<<input$selected_module>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        # If only module selected
        } else if (input$selected_module != "All" && input$selected_course == "All") {
          sel_apps <- unique(apps_init[apps_init$module == input$selected_module, "app"])
          # sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "module" : "<<input$selected_module>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        # If only course selected
        } else if (input$selected_module == "All" && input$selected_course != "All") {
          sel_apps <- unique(apps_init[apps_init$icourse == input$selected_course, "app"])
          # sel_apps <- try(sort(sdd_apps$distinct("app", query = glue::glue(r"--[{ "icourse" : "<<input$selected_course>>" }]--", .open = "<<", .close = ">>"))), silent = TRUE)
        # If nothing selected
        } else {
          sel_apps <- unique(apps_init$app)
          # sel_apps <- try(sort(sdd_apps$distinct("app")), silent = TRUE)
        }
        
        # Displaying the selector if apps didn't occur error
        if (!inherits(sel_apps, "try-error")) {
          tagList(
            tags$h3("Application :"),
            selectInput(ns("selected_app"), NULL, choices = c("All", sel_apps), selected = "All")
          )
        }
      }
    })
    
    # Display // Enrolled selector
    output$ui_enrolled_selector <- renderUI({
      
      if (!inherits(sdd_users2, "try-error")) {
      tagList(
        checkboxInput(ns("only_enrolled"), "Only Enrolled", value = TRUE)
      )
      } else { NULL }
    })
    
    # Display // User selector
    output$ui_login_selector <- renderUI({
      
      req(input$selected_course)
      
      users <- NULL
      # Getting the users with the function from the selectors
      if (!inherits(sdd_users2, "try-error")) {
        users <- try(get_users(input$selected_course, input$only_enrolled, users2_init, acad_year), silent = TRUE)
      }
      
      # If no errors to get the users : Display the selector
      if (!inherits(users, "try-error") && length(users > 0)) {
        tagList(
          tags$h3("Student :"),
          # Creation of selector with choices "All" and the users of course or all
          selectInput(ns("selected_user"), NULL, choices = c("All", users))
        )
      # Create an invisible selector with value NULL to get nothing from the request
      } else {
        tagList(
          tags$h3("Student :"),
          selectInput(ns("selected_user"), NULL, choices = "NULL")
        )
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
      
      if (!inherits(sdd_events, "try-error")) {
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
      input$selected_user
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
      # Preparing the apps and planning tables
      if (!inherits(sdd_apps, "try-error") && !inherits(sdd_planning, "try-error") && !inherits(sdd_courses, "try-error") && !inherits(sdd_modules, "try-error") && !inherits(sdd_users2, "try-error")) {
        courses_table <- courses_init
        modules_table <- modules_init
        apps_table <- apps_init
        planning_table <- planning_init
        users2_table <- users2_init
      }
      
      # [1] Creation of the request part for "course" if request there is
      if (is_request(input$selected_course)) {
        request_vector <- c(request_vector, "icourse" = glue::glue(r"--["icourse" : "<<input$selected_course>>"]--", .open = "<<", .close = ">>"))
        
        # Courses & Modules & Apps & Planning & users2 table icourse condition
        if (!inherits(sdd_apps, "try-error") && !inherits(sdd_planning, "try-error") && !inherits(sdd_courses, "try-error") && !inherits(sdd_modules, "try-error") && !inherits(sdd_users2, "try-error")) {
          courses_table <- courses_table[courses_table$icourse == input$selected_course,]
          modules_table <- modules_table[modules_table$icourse == input$selected_course,]
          apps_table <- apps_table[apps_table$icourse == input$selected_course,]
          planning_table <- planning_table[planning_table$icourse == input$selected_course,]
          users2_table <- users2_table[users2_table$icourse == input$selected_course,]
        }
      }
      
      # [2] Creation of the request part for "module" if request there is
      if (is_request(input$selected_module) && !is_request(input$selected_app)) {
        request_vector <- c(request_vector, "module" = glue::glue(r"--["module" : "<<input$selected_module>>"]--", .open = "<<", .close = ">>"))
        
        # Modules & Apps table module condition
        if (!inherits(sdd_apps, "try-error") && !inherits(sdd_modules, "try-error")) {
          modules_table <- modules_table[modules_table$module == input$selected_module,]
          apps_table <- apps_table[apps_table$module == input$selected_module,]
        }
      }
      
      # [3] Creation of the request part for "app" if request there is
      if (is_request(input$selected_app)) {
        request_vector <- c(request_vector, "app" = glue::glue(r"--["app" : "<<input$selected_app>>"]--", .open = "<<", .close = ">>"))
        
        # Apps table app condition
        if (!inherits(sdd_apps, "try-error")) {
          apps_table <- apps_table[apps_table$app == input$selected_app,]
        }
      }
      
      # [4] Creation of the request part for "user" if request there is
      if (is_request(input$selected_user)) {
        request_vector <- c(request_vector, "user" = glue::glue(r"--["user" : "<<input$selected_user>>"]--", .open = "<<", .close = ">>"))
        
        # Users2 table user condition
        if (!inherits(sdd_users2, "try-error")) {
          users2_table <- users2_table[users2_table$user == input$selected_user,]
        }
      }
      
      # [5] Creation of the request part for "dates" if request there is
      if (input$is_dates == TRUE) {
        # Preparation of the dates
        date_from <- paste0(input$selected_date1, " ", as.character(strftime(input$selected_time1, "%R")))
        date_from <- as.POSIXct(date_from, tz = "UTC")
        date_from <- format(date_from, "%Y-%m-%dT%H:%M:%SZ")
        date_to <- paste0(input$selected_date2, " ", as.character(strftime(input$selected_time2, "%R")))
        date_to <- as.POSIXct(date_to, tz = "UTC")
        date_to <- format(date_to, "%Y-%m-%dT%H:%M:%SZ")
        # Preparation of the request
        request_vector <- c(request_vector, "dates" = glue::glue(r"--["date" : { "$gte" : {"$date" : "<<date_from>>"} , "$lte" : {"$date" : "<<date_to>>"} }]--", .open = "<<", .close = ">>"))
        request_vector <- c(request_vector, "start_end" = glue::glue(r"--["start" : { "$lte" : {"$date" : "<<date_to>>"} } , "end" : { "$gte" : {"$date" : "<<date_from>>"} }]--", .open = "<<", .close = ">>"))
        
        # Courses & Modules & Apps & Planning table date condition
        if (!inherits(sdd_apps, "try-error") && !inherits(sdd_planning, "try-error") && !inherits(sdd_courses, "try-error") && !inherits(sdd_modules, "try-error")) {
          courses_table <- courses_table[courses_table$start < date_to & courses_table$end > date_from,]
          modules_table <- modules_table[modules_table$start < date_to & modules_table$end > date_from,]
          apps_table <- apps_table[apps_table$start < date_to & apps_table$end > date_from,]
          planning_table <- planning_table[planning_table$start < date_to & planning_table$end > date_from,]
        }
      }
      
      # Setting the tables in the reactiveVal(s) after every selection
      if (!inherits(sdd_apps, "try-error") && !inherits(sdd_planning, "try-error") && !inherits(sdd_courses, "try-error") && !inherits(sdd_modules, "try-error") && !inherits(sdd_users2, "try-error")) {
        apps(apps_table)
        planning(planning_table)
        courses(courses_table)
        modules(modules_table)
        users2(users2_table)
      }
      
      # If the vector is not null, return the vector, if it is, return "empty" to make empty request 
      if (!is.null(request_vector)) {
        request(request_vector)
      } else {
        request("empty")
      }
    })
    
    # Defining of main tables depending on the request
    observeEvent(request(), {
      # Variable : args used for the events tables
      events_args <- c("icourse", "module", "app", "user", "dates", "type")
      # apps_args <- c("icourse", "module", "app", "start_end")
      # planning_args <- c("icourse", "start_end")
      
      # --- Preparing the request for the events tables
      # 1 : Events
      events_request <- prepare_request(request(), events_args)
      
      # 2 : Apps
      # apps_request <- prepare_request(request(), apps_args)
      # 3 : Planning
      # planning_request <- prepare_request(request(), planning_args)
      
      print(events_request)
      # print(apps_request)
      
      # --- Preparing the logins from the users
      if (!inherits(sdd_users2, "try-error")) {
        all_users2 <- unique(users2_init[, c("user", "ilastname", "login")])
        users_login <- all_users2$login
        names(users_login) <- all_users2$user
      }
      # users2 <- try(unique(sdd_users2$find('{}', fields = '{"user" : true, "login" : true, "_id" : false}')), silent = TRUE)
      # if (!inherits(users2, "try-error")) {
      #   users_login <- users2$login
      #   names(users_login) <- users2$user
      # }
      
      # --- Requesting to databases
      events_table <- {message("requete events"); try(sdd_events$find(events_request, limit = 50000L), silent = TRUE)}
      if (!inherits(sdd_users2, "try-error")) {
        events_table$user <- as.character(users_login[events_table$user])
      }
      events(events_table)
      
      # {message("requete apps");apps(try(sdd_apps$find(apps_request), silent = TRUE))}
      # {message("requete planning");planning(try(sdd_planning$find(planning_request), silent = TRUE))}
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
      attr(news, "courses") <- try(sdd_events$distinct("icourse", query = request), silent = TRUE)
      return(news)
    })
    # Variable : learnr table with request for news
    learnr_news <- reactive({
      request <- prepare_request(news_request(), c("news_date", "type"), type = "learnr")
      news <- try(sdd_events$count(request), silent = TRUE)
      attr(news, "apps") <- try(sdd_events$distinct("app", query = request), silent = TRUE)
      attr(news, "courses") <- try(sdd_events$distinct("icourse", query = request), silent = TRUE)
      return(news)
    })
    # Variable : shiny table with request for news
    shiny_news <- reactive({
      request <- prepare_request(news_request(), c("news_date", "type"), type = "shiny")
      news <- try(sdd_events$count(request), silent = TRUE)
      attr(news, "apps") <- try(sdd_events$distinct("app", query = request), silent = TRUE)
      attr(news, "courses") <- try(sdd_events$distinct("icourse", query = request), silent = TRUE)
      return(news)
    })

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    right_sidebar_vars <- reactiveValues(
      selected_user = NULL,
      selected_course = NULL,
      events = NULL,
      courses = NULL,
      modules = NULL,
      apps = NULL,
      planning = NULL,
      users2 = NULL,
      h5p_news = NULL,
      learnr_news = NULL,
      shiny_news = NULL,
      selected_news_time = NULL,
      selected_module = NULL,
      selected_app = NULL,
    )
    
    # Updating the vars
    observe({
      right_sidebar_vars$selected_user <- input$selected_user
      right_sidebar_vars$selected_course <- input$selected_course
      right_sidebar_vars$events <- events()
      right_sidebar_vars$courses <- courses()
      right_sidebar_vars$modules <- modules()
      right_sidebar_vars$apps <- apps()
      right_sidebar_vars$planning <- planning()
      right_sidebar_vars$users2 <- users2()
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
