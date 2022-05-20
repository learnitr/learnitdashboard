#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Global parameter for modules
  all_vars <- reactiveValues(
    sdd_tables_vars = NULL,
    std_progression_vars = NULL,
  )
  
  # Updating the modules vars
  observe({
    all_vars$sdd_tables_vars <- sdd_tables_vars
  })
  observe({
    all_vars$std_progression_vars <- std_progression_vars
  })
  
  # Server module of 1st page
  sdd_tables_vars <- mod_sdd_tables_server("sdd_tables_1", all_vars = all_vars)
  # Server module of 2nd page
  std_progression_vars <- mod_std_progression_server("std_progression_1", all_vars = all_vars)
  
}
