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
    
    fluidRow(
      
      column( width = 3,
        uiOutput(ns("dt_cols_selector")),
      ),
    
      column( width = 3,
        uiOutput(ns("sdd_login_selector")),
      ),
    ),
    
    fluidRow(
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
    
    # === H5P Variables ===
    # Variable : H5P
    h5p <- reactiveVal()
    try(h5p({message("requete h5p");sdd_h5p$find('{}', limit = 1000)}))
    
    # === Learnr Variables ===
    # Variable : Learnr
    learnr <- reactiveVal()
    try(learnr({message("requete learnr");sdd_learnr$find('{}', limit = 1000)}))
    
    # === Shiny Variables ===
    # Variable : Shiny
    shiny <- reactiveVal()
    try(shiny({message("requete shiny");sdd_shiny$find('{}', limit = 1000)}))
    
    # === SDD Variables ===
    # Variable : Logins
    sdd_logins <- reactive({
      try((unique(h5p()["login"])))
    })

# Reactive Values ---------------------------------------------------------

    # Variable : Definition of the request, "All" or only one selected login
    request <- reactive({
      # Definition of the request : All or only one selected login
      if (req(input$sdd_selected_login) != "All" ) {
        return(paste0( r"( {"login" : ")", input$sdd_selected_login, r"("} )"))
      } else {
        return("{}")
      }
    })
    
# DT Displays -------------------------------------------------------------
    
    # Display // DT cols selector
    output$dt_cols_selector <- renderUI({
      # If there was no error while loading the table
      if (!inherits(h5p(), "try-error")) {
        tagList(
          tags$h3("Columns :"),
          # Creation of the selector of cols to show, because the page is too small for everything
          selectInput(ns("dt_selected_cols"), NULL, choices = c("All",names(h5p())), multiple = TRUE, selected = "All")
        )
      } else { NULL }
    })
    
    # Display // Login selector
    output$sdd_login_selector <- renderUI({
      # If there was no error while getting the logins
      if (!inherits(sdd_logins(), "try-error")) {
        tagList(
          tags$h3("Login :"),
          # Creation of selector with choices "All" and the logins
          selectInput(ns("sdd_selected_login"), NULL, choices = c("All", sdd_logins()))
        )
      } else { NULL }
    })
    
    # Display // H5P datatable
    output$sdd_h5p_dt <- renderDT({
      
      # Getting the dataframe of defined request
      h5p <- sdd_h5p$find(request(), limit = 1000)
      
      # Displaying everything or only selected cols
      if (req(input$dt_selected_cols) == "All") {
        h5p
      } else {
        h5p[req(input$dt_selected_cols)]
      }
    })
    
    # Display // Learnr datatable
    output$sdd_learnr_dt <- renderDT({
      
      # Getting the dataframe of defined request
      learnr <- sdd_learnr$find(request(), limit = 1000)
      
      # Displaying everything or only selected cols
      if (req(input$dt_selected_cols) == "All") {
        learnr
      } else {
        learnr[req(input$dt_selected_cols)]
      }
    })
    
    # Display // Shiny datatable
    output$sdd_shiny_dt <- renderDT({
      
      # Getting the dataframe of defined request
      shiny <- sdd_shiny$find(request(), limit = 1000)
      
      # Displaying everything or only selected cols
      if (req(input$dt_selected_cols) == "All") {
        shiny
      } else {
        shiny[req(input$dt_selected_cols)]
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_sdd_tables_ui("sdd_tables_1")
    
## To be copied in the server
# mod_sdd_tables_server("sdd_tables_1")
