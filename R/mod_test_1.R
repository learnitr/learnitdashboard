#' test_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_1_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    tags$br(),
    uiOutput(ns("dt_cols_selector")),
    tags$br(),
    
    tabsetPanel(id = ns("activetab"),
      # First tab
      tabPanel("H5P",
        tags$br(),
        DTOutput(ns("sdd_h5p_dt")),
      ),
      # Second tab
      tabPanel("Learnr",
        tags$br(),
        DTOutput(ns("sdd_learnr_dt")),
      ),
      # Third tab
      tabPanel("Shiny",
        tags$br(),
        DTOutput(ns("sdd_shiny_dt")),
      ),
    ),
    
  )
}
    
#' test_1 Server Functions
#'
#' @noRd 
mod_test_1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
# Global Vars -------------------------------------------------------------
    
    # URL pour accéder à la base de données
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # Pour se connecter
    sdd_h5p <- try(mongolite::mongo("h5p", url = sdd_url))
    sdd_learnr <- try(mongolite::mongo("learnr", url = sdd_url))
    sdd_shiny <- try(mongolite::mongo("shiny", url = sdd_url))
    
    # Variable : H5P
    h5p <- reactiveVal()
    h5p(sdd_h5p$find('{}', limit = 10))
    # Variable : Learnr
    learnr <- reactiveVal()
    learnr(sdd_learnr$find('{}', limit = 10))
    # Variable : Shiny
    shiny <- reactiveVal()
    shiny(sdd_shiny$find('{}', limit = 10))
    

# DT Displays -------------------------------------------------------------
    
    # Display // DT cols selector
    output$dt_cols_selector <- renderUI({
      dt <- switch (input$activetab,
        "H5P" = h5p(),
        "Learnr" = learnr(),
        "Shiny" = shiny()
      )
      selectInput(ns("dt_selected_cols"), NULL, choices = names(dt), multiple = TRUE)
    })
    
    # Display // H5P datatable
    output$sdd_h5p_dt <- renderDT(
      h5p()[req(input$dt_selected_cols)], options = list(
        lengthChange = FALSE,
        pageLength = 5
      )
    )
    # Display //
    output$sdd_learnr_dt <- renderDT(
      learnr()[req(input$dt_selected_cols)], options = list(lengthChange = FALSE)
    )
    # Display //
    output$sdd_shiny_dt <- renderDT(
      shiny()[req(input$dt_selected_cols)], options = list(lengthChange = FALSE)
    )
    
  })
}
    
## To be copied in the UI
# mod_test_1_ui("test_1_1")
    
## To be copied in the server
# mod_test_1_server("test_1_1")
