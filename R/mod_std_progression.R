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
    uiOutput(ns("stdp_login_selector")),
    ),
    # Box to render the progression
    box( title = "Graph :", status = "primary", solidHeader = TRUE, width = 8,
    uiOutput(ns("stdp_progression")),
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


# Tables ------------------------------------------------------------------

    # Variable : H5P table
    h5p <- reactiveVal()
    # Variable : Learnr table
    learnr <- reactiveVal()
    # Variable : shiny table
    shiny <- reactiveVal()
    
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
    
# Request ----------------------------------------------------------------

    # Variable : Definition of the request, "All" or only one selected login
    request <- eventReactive({
      input$stdp_selected_login
    }, {
      # Is there a login request ?
      login_request <- !is.null(input$stdp_selected_login) && input$stdp_selected_login != "All"
      # Creation of the request part for login
      if (login_request) {
        login_querry <- glue::glue(r"("login" : "<<input$stdp_selected_login>>")", .open = "<<", .close = ">>")
      }
      
      # Definition of the request
      # Request of login and app
      if (login_request) {
        request <- r"({<<login_querry>>})"
        # No special request
      } else {
        request <- "{}"
      }
      # Send the request after evaluating it
      return(glue::glue(request, .open = "<<", .close = ">>"))
    })
    
    # Defining tables depending on the request
    observeEvent(request(), {
      print(request())
      {message("requete h5p");h5p(try(sdd_h5p$find(request(), limit = 1000), silent = TRUE))}
      {message("requete learnr");learnr(try(sdd_learnr$find(request(), limit = 1000), silent = TRUE))}
      {message("requete shiny");shiny(try(sdd_shiny$find(request(), limit = 1000), silent = TRUE))}
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
