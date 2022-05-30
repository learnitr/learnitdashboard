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
    
    # Displaying the data tables
    column( width = 12,
      DTOutput(ns("sdd_dt")),
    ),
    
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
    selected_table <- reactive({all_vars$right_sidebar_vars$selected_table})
    h5p <- reactive({all_vars$right_sidebar_vars$h5p})
    learnr <- reactive({all_vars$right_sidebar_vars$learnr})
    shiny <- reactive({all_vars$right_sidebar_vars$shiny})
    
# DT Displays -------------------------------------------------------------
    
    # Display // H5P or Learnr or Shiny datatable
    output$sdd_dt <- renderDT({
      
      req(selected_table())
      
      # Getting the right table depending on the tab
      table <- switch (selected_table(),
                       "H5P" = h5p(),
                       "Learnr" = learnr(),
                       "Shiny" = shiny()
      )
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(table, "try-error") && length(table > 0)) {
        # The columns selection is now rendered by DT !
        table
      } else {
        NULL
      }
    },
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX = TRUE,
      pageLength = 100,
      lengthMenu = c(50,100,200,500),
      dom = 'Bfrtip',
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
