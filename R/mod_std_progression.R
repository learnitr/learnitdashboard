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
    
    # Box to select the login
    box( title = "Login :", status = "warning", solidHeader = TRUE, width = 4,
    uiOutput(ns("stdp_login_selector"))
    ),
    # Box to render the progression
    box( title = "Graph :", status = "primary", solidHeader = TRUE, width = 8,
    uiOutput(ns("stdp_progression"))
    )
    
  )
}
    
#' std_progression Server Functions
#'
#' @noRd 
mod_std_progression_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
# Getting Modules Vars ----------------------------------------------------
    
    # Vars from sdd_tables
    logins <- reactive({all_vars$sdd_tables_vars$logins})
    sdd_selected_login <- reactive({all_vars$sdd_tables_vars$sdd_selected_login})
    h5p <- reactive({all_vars$sdd_tables_vars$h5p})
    
# Selectors ---------------------------------------------------------------
    
    # Display // Login selector
    output$stdp_login_selector <- renderUI({
      # If there was no error while getting the logins
      if (!is.null(logins())) {
        tagList(
          # tags$h3("Login :"),
          # Creation of selector with choices "All" and the logins
          selectInput(ns("stdp_selected_login"), NULL, choices = c("All", logins()), selected = sdd_selected_login())
        )
      } else { NULL }
    })
    
# Display Progression -----------------------------------------------------

    # Display the progression of selected student or message if all selected
    output$stdp_progression <- renderUI({
      req(input$stdp_selected_login)
      
      # Message if nothing selected
      if (input$stdp_selected_login == "All") {
        return(tags$h4("Nothing to display, please select a login."))
      # Progression if login selected
      } else if (input$stdp_selected_login != "All") {
        return(plotOutput(ns("test_graph")))
      }
    })
    
    # Display test graph
    output$test_graph <- renderPlot({
      if (req(input$stdp_selected_login) != "All") {
        login_grades <- na.omit(h5p()[c("login", "app", "grade")])
        ggplot2::ggplot(login_grades) +
          geom_count(mapping = aes(x = grade, y = app))
      }
    })

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    std_progression_vars <- reactiveValues(
      stdp_selected_login = NULL,
    )
    
    # Updating the vars
    observe({
      std_progression_vars$stdp_selected_login <- input$stdp_selected_login
    })
    
    # Sending the vars
    return(std_progression_vars)    
    
  })
}
    
## To be copied in the UI
# mod_std_progression_ui("std_progression_1")
    
## To be copied in the server
# mod_std_progression_server("std_progression_1")
