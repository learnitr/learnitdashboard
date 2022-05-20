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
    
    # Box to select the course
    box( title = "Course :", status = "warning", solidHeader = TRUE,width = 4,
         uiOutput(ns("cls_course_selector")),
    ),
    # Box to render the course progression
    box( title = "Graph :", status = "primary", solidHeader = TRUE, width = 8,
         uiOutput(ns("cls_course_progression")),
    )
    
  )
}
    
#' cls_progression Server Functions
#'
#' @noRd 
mod_cls_progression_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Global Vars -------------------------------------------------------------
    
    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # Connection to Users
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    # Variable : All the existing classes
    courses <- try(sort(unique(sdd_users$distinct("ictitle"))), silent = TRUE)
    if (inherits(courses, "try-error") || length(courses) == 0) {
      courses <- NULL
    }
    # Disconnecting from the database
    try(sdd_users$disconnect(), silent = TRUE)
    

# Selectors ---------------------------------------------------------------
    
    # Display // Course selector
    output$cls_course_selector <- renderUI({
      # If there was no error while getting the courses
      if (!is.null(courses)) {
        # Creation of selector with choices "All" and the courses
        tagList(
          selectInput(ns("cls_selected_course"), NULL, choices = c("All", courses), selected = "All")
        )
      } else { NULL }
    })

# Display Course Progression -------------------------------------------------------------

    # Display the progression of selected student or message if all selected
    output$cls_course_progression <- renderUI({
      req(input$cls_selected_course)
      
      # Message if nothing selected
      if (input$cls_selected_course == "All") {
        return(tags$h4("Nothing to display, please select a course"))
        # Progression if login selected
      } else if (input$cls_selected_course != "All") {
        return(plotOutput(ns("test_graph")))
      }
    })
    
    # Display test graph
    output$test_graph <- renderPlot({
      if (req(input$cls_selected_course) != "All") {
        plot(rnorm(30))
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_cls_progression_ui("cls_progression_1")
    
## To be copied in the server
# mod_cls_progression_server("cls_progression_1")
