#' apps_progression UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_apps_progression_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$h4("hello")
  )
}
    
#' apps_progression Server Functions
#'
#' @noRd 
mod_apps_progression_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_apps_progression_ui("apps_progression_1")
    
## To be copied in the server
# mod_apps_progression_server("apps_progression_1")
