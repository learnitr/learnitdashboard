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
      uiOutput(ns("apps_graph_2")),
    ),
    
    fluidRow(
      # Graph 3
      uiOutput(ns("apps_graph_3")),
      
      # Graph 4
      uiOutput(ns("apps_graph_4")),
    )
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
    is_dates_or_user <- reactive({
      is_dates() || (selected_user() != "All" && selected_user() != "NULL")
    })

# Graph 1 -----------------------------------------------------------------

    # Rendering the box and the outputs
    output$apps_graph_1 <- renderUI({
      if (!inherits(sdd_events, "try-error")) {
        tagList(
          box( title = "Apps Graph 1", solidHeader = TRUE,
               width = 5, collapsible = TRUE, status = "purple",
               checkboxInput(ns("g1_is_na"), "With NA's"),
               plotlyOutput(ns("graph_1"))
          )
        )
      }
    })
    
    # Rendering the graph
    output$graph_1 <- renderPlotly({
      # Setting the data to NULL
      data <- NULL
      # Data preparation if only course and not date and events is not empty, then get the data
      if ((is_course() || is_module()) && !is_dates_or_user() && nrow(events()) > 0) {
        # Only the elements that are submitted or evaluated or answered
        data <- events()[events()$verb == "submitted" | events()$verb == "evaluated" | events()$verb == "answered", c("app", "correct")]
        # If not with NA's, na.omit()
        if (!input$g1_is_na) {
          data <- na.omit(data)
        }
      }
      
      # Creation of the result graph
      if (!is.null(data) && nrow(data) > 0) {
        ggplot(data = data, mapping = aes(x = app, fill = correct)) +
          xlab("Apps") +
          ylab("Amount of Answers") +
          coord_flip() +
          geom_bar()
      }
    })

# Graph 2 -----------------------------------------------------------------

    # Rendering the box and the outputs
    output$apps_graph_2 <- renderUI({
      if (!inherits(sdd_events, "try-error")) {
        tagList(
          box( title = "Apps Graph 2", solidHeader = TRUE,
               width = 5, collapsible = TRUE, status = "purple",
               "Content"
          )
        )
      }
    })

# Graph 3 -----------------------------------------------------------------

    # Rendering the box and the outputs
    output$apps_graph_3 <- renderUI({
      if (!inherits(sdd_events, "try-error")) {
        tagList(
          box( title = "Apps Graph 3", solidHeader = TRUE,
               width = 5, collapsible = TRUE, status = "purple",
               "Content"
          )
        )
      }
    })

# Graph 4 -----------------------------------------------------------------

    # Rendering the box and the outputs
    output$apps_graph_4 <- renderUI({
      if (!inherits(sdd_events, "try-error")) {
        tagList(
          box( title = "Apps Graph 4", solidHeader = TRUE,
               width = 5, collapsible = TRUE, status = "purple",
               "Content"
          )
        )
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_apps_progression_ui("apps_progression_1")
    
## To be copied in the server
# mod_apps_progression_server("apps_progression_1")
