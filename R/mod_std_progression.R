#' std_progression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_std_progression_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # uiOutput to display the progression of the selected student
    uiOutput(ns("stdp_progression"))
    
  )
}
    
#' std_progression Server Functions
#'
#' @noRd 
mod_std_progression_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
# Getting Modules Vars ----------------------------------------------------
    
    # Vars from right_sidebar
    selected_login <- reactive({all_vars$right_sidebar_vars$selected_login})
    h5p <- reactive({all_vars$right_sidebar_vars$h5p})
    
# Display Progression -----------------------------------------------------

    # Display the progression of selected student or message if all selected
    output$stdp_progression <- renderUI({
      req(selected_login())
      
      # Message if nothing selected
      if (selected_login() == "All") {
        tagList(
          # Dashboard box
          box( title = "Graph :", status = "primary", solidHeader = TRUE, width = 8,
            tags$h4("Nothing to display, please select a login.")
          )
        )
      # Progression if login selected
      } else if (selected_login() != "All") {
        tagList(
          # Dashboard box
          box( title = paste0("Graph : ",selected_login()), status = "primary", solidHeader = TRUE, width = 8,
            plotOutput(ns("test_graph"))
          )
        )
      }
    })
    
    # Display test graph
    output$test_graph <- renderPlot({
      if (req(selected_login()) != "All") {
        plot(rnorm(30))
      }
    })

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    std_progression_vars <- reactiveValues()
    
    # Updating the vars
    # Nothing yet
    
    # Sending the vars
    return(std_progression_vars)    
    
  })
}
    
## To be copied in the UI
# mod_std_progression_ui("std_progression_1")
    
## To be copied in the server
# mod_std_progression_server("std_progression_1")
