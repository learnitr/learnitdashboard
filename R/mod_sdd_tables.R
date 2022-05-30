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
    
    tabBox( title = "Raw Data Exploration", width = 12,
      # Display of the result H5P table
      tabPanel("H5P",
        DTOutput(ns("sdd_dt_h5p")),
      ),
      # Display of the result Learnr table
      tabPanel("Learnr",
        DTOutput(ns("sdd_dt_learnr")),
      ),
      # Display of the result Shiny table
      tabPanel("Shiny",
        DTOutput(ns("sdd_dt_shiny")),
      )
    )
    
  )
}
    
#' sdd_tables Server Functions
#'
#' @noRd 
mod_sdd_tables_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------
    
    # Vars from right_sidebar
    h5p <- reactive({all_vars$right_sidebar_vars$h5p})
    learnr <- reactive({all_vars$right_sidebar_vars$learnr})
    shiny <- reactive({all_vars$right_sidebar_vars$shiny})
    
# DT Displays -------------------------------------------------------------
    
    # Display // H5P datatable
    output$sdd_dt_h5p <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(h5p(), "try-error") && length(h5p() > 0)) {
        # The columns selection is now rendered by DT !
        h5p()
      } else {
        NULL
      }
    },
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 100,
      lengthMenu = c(50,100,200,500),
      # Options to get the col selector and lengthmenu
      dom = 'Blfrtip',
      buttons = I('colvis')
    )
    )
    
    # Display // Learnr datatable
    output$sdd_dt_learnr <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(learnr(), "try-error") && length(learnr() > 0)) {
        # The columns selection is now rendered by DT !
        learnr()
      } else {
        NULL
      }
    },
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 100,
      lengthMenu = c(50,100,200,500),
      # Options to get the col selector and lengthmenu
      dom = 'Blfrtip',
      buttons = I('colvis')
    )
    )
    
    # Display // Shiny datatable
    output$sdd_dt_shiny <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(shiny(), "try-error") && length(shiny() > 0)) {
        # The columns selection is now rendered by DT !
        shiny()
      } else {
        NULL
      }
    },
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 100,
      lengthMenu = c(50,100,200,500),
      # Options to get the col selector and lengthmenu
      dom = 'Blfrtip',
      buttons = I('colvis')
    )
    )

# Communication -----------------------------------------------------------

    # Variable : all of module's vars
    sdd_tables_vars <- reactiveValues()
    
    # Updating the vars
    # Nothing yet
    
    # Sending the vars
    return(sdd_tables_vars)
    
  })
}
    
## To be copied in the UI
# mod_sdd_tables_ui("sdd_tables_1")
    
## To be copied in the server
# mod_sdd_tables_server("sdd_tables_1")
