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
          box( title = "Graph :", status = "primary", solidHeader = TRUE, width = 8,
            tags$h4("Nothing to display, please select a course")
          )
        )
        # Progression if login selected
      } else if (selected_course() != "All") {
        tagList(
          # Dashboard Box
          box( title = paste0("Graph : ", selected_course()), status = "primary", solidHeader = TRUE, width = 8,
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
      # sdd_h5p <-try(mongolite::mongo("h5p", url = "mongodb://127.0.0.1:27017/sdd"), silent = TRUE)
      # h5pt <- try(sdd_h5p$find(query = '{ "grade" : { "$gte" : 0}}', fields = '{ "login" : true, "grade" : true, "app" : true}', limit = 2000), silent = TRUE)
      # sdd_h5p$disconnect()
      # return(
      # ggplot(data = h5pt) +
      #   geom_bar(mapping = aes(grade))+
      #   theme_bw() +
      #   facet_trelliscope(~ login, nrow = 2, ncol = 5, width = 300)
      # )
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
