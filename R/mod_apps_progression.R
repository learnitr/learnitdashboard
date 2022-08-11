#' apps_progression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_apps_progression_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      # Graph 1
      uiOutput(ns("prog_graph_1")),
      # Graph 2
      uiOutput(ns("prog_graph_2"))
      
    ),
    
  )
}
    
#' apps_progression Server Functions
#'
#' @noRd 
mod_apps_progression_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------

    # Vars from right_sidebar
    selected_course <- reactive({all_vars$right_sidebar_vars$selected_course})
    selected_module <- reactive({all_vars$right_sidebar_vars$selected_module})
    selected_app <- reactive({all_vars$right_sidebar_vars$selected_app})
    selected_user <- reactive({all_vars$right_sidebar_vars$selected_user})
    is_dates <- reactive({all_vars$right_sidebar_vars$is_dates})
    
    events <- reactive({all_vars$right_sidebar_vars$events})
    
    request <- reactive({all_vars$right_sidebar_vars$request})

# Global Vars -------------------------------------------------------------

    # Variable : Is only the course selected
    is_course <- reactive({
      selected_course() != "All" && selected_module() == "All" && selected_app() == "All"
    })
    # Variable : Is a module selected
    is_module <- reactive({
      selected_module() != "All" && selected_app() == "All"
    })
    # Variable : Is an app selected
    is_app <- reactive({
      selected_app() != "All"
    })
    # Variable : Is dates or an user selected ?
    # is_dates_or_user <- reactive({
    #   is_dates() || (selected_user() != "All" && selected_user() != "NULL")
    # })
    # Variable : Is user ?
    is_user <- reactive({
      selected_user() != "All" && selected_user() != "NULL"
    })
    
    # Variable : Data for the app progression graph
    app_prog_data <- reactive({
      
      # Initialisation of the request vector
      request_vector <- c()
      
      # Request part for the course
      if (is_request(selected_course())) {
        request_vector <- c(request_vector, glue::glue(r"--["icourse" : "<<selected_course()>>"]--", .open = "<<", .close = ">>"))
      }
      # Request part for the module
      if (is_request(selected_module())) {
        request_vector <- c(request_vector, glue::glue(r"--["module" : "<<selected_module()>>"]--", .open = "<<", .close = ">>"))
      }
      # Request part for the app
      if (is_request(selected_app())) {
        request_vector <- c(request_vector, glue::glue(r"--["app" : "<<selected_app()>>"]--", .open = "<<", .close = ">>"))
      }
      # Request part for the user
      if (is_request(selected_user())) {
        request_vector <- c(request_vector, glue::glue(r"--["user" : "<<selected_user()>>"]--", .open = "<<", .close = ">>"))
      }
      # Creation of the entire request part if the vector isn't NULL
      if (!is.null(request_vector)) {
        conditions <- paste(request_vector, collapse = ", ")
      } else { conditions <- NULL}
      
      # If it's only a selected course -> grouped by module, if else -> grouped by app
      app_or_module <- ifelse(is_course() && !is_user(), "$module", "$app")
      
      # Creation of the entire request if the request part isn't NULL
      if (!is.null(conditions)) {
        request <- r"--[
        [{ "$match" : {
        <<conditions>>,
        "verb" : {"$in" : ["submitted", "answered"]},
        "correct" : {"$in" : [true, false]}
        }},
        { "$group" : {
        "_id" : "<<app_or_module>>",
        "correct" : {"$sum" : { "$cond" : [ { "$eq" : ["$correct", true] }, 1, 0 ] } },
        "incorrect" : {"$sum" : { "$cond" : [ { "$eq" : ["$correct", false] }, 1, 0 ] } }
        }}]
        ]--"
        request <- glue::glue(request, .open = "<<", .close = ">>")
      } else { request <- NULL }
      
      # Getting the data
      if (!inherits(sdd_events, "try-error") && !is.null(request)) {
        # Making the request
        data <- sdd_events$aggregate(request)
      } else { data <- NULL }
      
      # Preparation of the data
      if (!is.null(data) && nrow(data) > 0) {
        # Setting the good names
        if (app_or_module == "$module") {
          names(data) <- c("module", "correct", "incorrect")
        } else {
          names(data) <- c("app", "correct", "incorrect")
        }
        # Getting the amount of students for the selection
        if (!is.null(selected_course())) {
          nb_std <- length(unique(users2_init[users2_init$icourse == selected_course(), "user"]))
        } else {
          nb_std <- length(unique(users2_init["user"]))
        }
        # Counting the answers / nb_students
        data$correct <- round(data$correct / nb_std, 2)
        data$incorrect <- round(data$incorrect / nb_std, 2)
        data <- tidyr::pivot_longer(data, cols = c(correct, incorrect), names_to = "correct", values_to = "count")
        return(data)
      }
    })

# Graph 1 -----------------------------------------------------------------
    
    # Rendering the box and the outputs
    output$prog_graph_1 <- renderUI({
      if (is.null(app_prog_data())) {
        tagList(
          box( title = "Apps Progression Graph" , solidHeader = TRUE,
               width = 6, collapsible = TRUE, status = "info",
               tags$h4("Nothing to display... Please select other values.")
          )
        )
      } else if (!inherits(sdd_events, "try-error") && nrow(req(app_prog_data())) > 0) {
        # Creating the title
        title <- c("Apps Progression Graph")
        if (is_course()) {
          title <- c(title, paste0("Course : ", selected_course()))
        } else if (is_module()) {
          title <- c(title, paste0("Module : ", selected_module()))
        } else if (is_app()) {
          title <- c(title, paste0("App : ", selected_app()))
        }
        if (is_user()) {
          title <- c(title, paste0("User : ", unique(users2_init[users2_init$user == selected_user(), "login"])))
        }
        tagList(
          box( title = paste(title, collapse = " / ") , solidHeader = TRUE,
               width = 6, collapsible = TRUE, status = "info",
               plotly::plotlyOutput(ns("graph_1"))
          )
        )
      }
    })
    
    # Rendering the graph
    output$graph_1 <- plotly::renderPlotly({
      
      # If the data is available and not empty
      if (nrow(req(app_prog_data())) > 0) {
        
        # If it's from a course, and thus show modules progression
        if ("module" %in% names(app_prog_data())) {
          # Creation of the result graph
          ggplot(data = app_prog_data(), mapping = aes(x = module, y = count, fill = correct)) +
            xlab("Modules") +
            ylab("Count") +
            coord_flip() +
            geom_bar(stat = "identity")
        # Or if it's from something esle, and thus show apps progression
        } else {
          # Creation of the result graph
          ggplot(data = app_prog_data(), mapping = aes(x = app, y = count, fill = correct)) +
            xlab("Apps") +
            ylab("Count") +
            coord_flip() +
            geom_bar(stat = "identity")
        }
      } else {
        NULL
      }
    })

# Graph 2 -----------------------------------------------------------------

    # Display of the output for Graph 2
    output$prog_graph_2 <- renderUI({
      req(selected_course())
      
      # Message if nothing selected
      if (selected_course() == "All") {
        tagList(
          # Dashboard Box
          box( title = "Template Graph (based on course) :", status = "info", solidHeader = TRUE, width = 6, collapsible = TRUE,
               tags$h4("Nothing to display, please select a course.")
          )
        )
        # Progression if login selected
      } else if (selected_course() != "All") {
        tagList(
          # Dashboard Box
          box( title = paste0("Template Graph (based on course) : ", selected_course()), status = "info", solidHeader = TRUE, width = 6, collapsible = TRUE,
               plotOutput(ns("template_graph"))
          )
        )
      }
    })
    
    # Display test graph
    output$template_graph <- renderPlot({
      if (req(selected_course()) != "All") {
        plot(rnorm(30))
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_apps_progression_ui("apps_progression_1")
    
## To be copied in the server
# mod_apps_progression_server("apps_progression_1")
