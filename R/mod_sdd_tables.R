#' sdd_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sdd_tables_ui <- function(id){
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
    
#' sdd_tables Server Functions
#'
#' @noRd 
mod_sdd_tables_server <- function(id){
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
    try(h5p({message("requete h5p");sdd_h5p$find('{}', limit = 10)}))
    # Variable : Learnr
    learnr <- reactiveVal()
    try(learnr(sdd_learnr$find('{}', limit = 10)))
    # Variable : Shiny
    shiny <- reactiveVal()
    try(shiny(sdd_shiny$find('{}', limit = 10)))
    
    
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
    # Display // Learnr datatable
    output$sdd_learnr_dt <- renderDT(
      learnr()[req(input$dt_selected_cols)], options = list(lengthChange = FALSE)
    )
    # Display // Shiny datatable
    output$sdd_shiny_dt <- renderDT(
      shiny()[req(input$dt_selected_cols)], options = list(lengthChange = FALSE)
    )
    
  })
}
    
## To be copied in the UI
# mod_sdd_tables_ui("sdd_tables_1")
    
## To be copied in the server
# mod_sdd_tables_server("sdd_tables_1")
