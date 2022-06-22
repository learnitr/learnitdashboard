#' cls_progression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cls_progression_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # uiOutput to display the progrssion of the selected course
    uiOutput(ns("cls_course_progression"))
    
  )
}
    
#' cls_progression Server Functions
#'
#' @noRd 
mod_cls_progression_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------

    # Vars from right sidebar
    selected_course <- reactive({all_vars$right_sidebar_vars$selected_course})

# Display Course Progression -------------------------------------------------------------

    # Display the progression of selected student or message if all selected
    output$cls_course_progression <- renderUI({
      req(selected_course())
      
      # Message if nothing selected
      if (selected_course() == "All") {
        tagList(
          # Dashboard Box
          box( title = "Graph :", status = "info", solidHeader = TRUE, width = 8,
            tags$h4("Nothing to display, please select a course.")
          )
        )
        # Progression if login selected
      } else if (selected_course() != "All") {
        tagList(
          # Dashboard Box
          box( title = paste0("Graph : ", selected_course()), status = "info", solidHeader = TRUE, width = 8,
            plotOutput(ns("test_graph"))
          )
        )
      }
    })
    
    # Display test graph
    output$test_graph <- renderPlot({
      if (req(selected_course()) != "All") {
        plot(rnorm(30))
      }
    })

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    cls_progression_vars <- reactiveValues()
    
    # Updating the vars
    # Nothing yet
    
    # Sending the vars
    return(cls_progression_vars)
    
  })
}
    
## To be copied in the UI
# mod_cls_progression_ui("cls_progression_1")
    
## To be copied in the server
# mod_cls_progression_server("cls_progression_1")
