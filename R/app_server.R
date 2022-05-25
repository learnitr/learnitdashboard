#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  # Global var to communicate between modules
  all_vars <- reactiveValues(
    right_sidebar_vars = NULL,
    std_progression_vars = NULL,
    cls_progression_vars = NULL,
    sdd_tables_vars = NULL,
  )
  
  # Updating the modules vars
  observe({
    all_vars$right_sidebar_vars <- right_sidebar_vars
  })
  observe({
    all_vars$std_progression_vars <- std_progression_vars
  })
  observe({
    all_vars$cls_progression_vars <- cls_progression_vars
  })
  observe({
    all_vars$sdd_tables_vars <- sdd_tables_vars
  })
  
  # Server module of the right sidebar for selectors
  right_sidebar_vars <- mod_right_sidebar_server("right_sidebar_1", all_vars = all_vars)
  # Server module of 1st page
  std_progression_vars <- mod_std_progression_server("std_progression_1", all_vars = all_vars)
  # Server module of 2nd page
  cls_progression_vars <- mod_cls_progression_server("cls_progression_1", all_vars = all_vars)
  # Server module of 3rd page
  sdd_tables_vars <- mod_sdd_tables_server("sdd_tables_1", all_vars = all_vars)
  
}
