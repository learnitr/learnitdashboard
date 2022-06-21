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
      uiOutput(ns("apps_graph_1")),
      
      # Graph 2
      uiOutput(ns("apps_graph_3")),
    ),
    
    # fluidRow(
    #   # Graph 3
    #   uiOutput(ns("apps_graph_3"))
    # )
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

# Graph 1 -----------------------------------------------------------------

    # Rendering the box and the outputs
    output$apps_graph_1 <- renderUI({
      if (!inherits(sdd_events, "try-error")) {
        # Creating the title
        title <- c("Apps Graph")
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
               width = 5, collapsible = TRUE, status = "purple",
               plotly::plotlyOutput(ns("graph_1"))
          )
        )
      }
    })
    
    # Rendering the graph
    output$graph_1 <- plotly::renderPlotly({
      if (nrow(req(app_prog_data())) > 0) {
        
        # Creation of the result graph
        ggplot(data = app_prog_data(), mapping = aes(x = app, fill = correct)) +
          xlab("Apps") +
          ylab("Amount of Answers") +
          coord_flip() +
          geom_bar()
      }
    })

# Request -----------------------------------------------------------------

    # Variable : App Progression graph data
    app_prog_data <- reactiveVal()
    
    # Update of the variable
    observeEvent(request(), {
      # Creation of a special request for the app progression graph wih the good verbs
      app_prog_request <- c(request(), "verbs" = r"--["verb" : {"$in" : ["submitted", "answered"]}]--")
      app_prog_request <- prepare_request(app_prog_request, c("icourse", "module", "app", "user", "verbs"))
      
      if (app_prog_request != "{}") {
        # Getting the data, only the app and correct fields
        data <- try(na.omit(sdd_events$find(query = app_prog_request, fields = '{"_id" : false, "app" : true, "correct" : true}')), silent = TRUE)
        if (!inherits(data, "try-error") && !is.null(data) && nrow(data) > 0) {
          data <- data[order(data$app),]
          # Putting the data inside of the reactive Variable
          app_prog_data(data)
        }
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_apps_progression_ui("apps_progression_1")
    
## To be copied in the server
# mod_apps_progression_server("apps_progression_1")
