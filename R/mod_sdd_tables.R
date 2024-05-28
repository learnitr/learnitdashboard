#' sdd_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sdd_tables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    tabBox( title = "Raw Data Exploration", width = 12,
      # Display of the result Events table
      tabPanel("Events", DTOutput(ns("sdd_dt_events"))),
      # Display of the result Courses table
      tabPanel("Courses", DTOutput(ns("sdd_dt_courses"))),
      # Display of the result Modules table
      tabPanel("Modules", DTOutput(ns("sdd_dt_modules"))),
      # Display of the result Apps table
      tabPanel("Apps", DTOutput(ns("sdd_dt_apps"))),
      # Display of the result Planning table
      tabPanel("Planning", DTOutput(ns("sdd_dt_planning"))),
      # Display of the result Users table
      tabPanel("Users", DTOutput(ns("sdd_dt_users2")))
    )
  )
}
    
#' sdd_tables Server Functions
#'
#' @noRd 
mod_sdd_tables_server <- function(id, all_vars) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------
    
    # Vars from right_sidebar
    events   <- reactive({all_vars$right_sidebar_vars$events})
    courses  <- reactive({all_vars$right_sidebar_vars$courses})
    modules  <- reactive({all_vars$right_sidebar_vars$modules})
    apps     <- reactive({all_vars$right_sidebar_vars$apps})
    planning <- reactive({all_vars$right_sidebar_vars$planning})
    users2   <- reactive({all_vars$right_sidebar_vars$users2})
  
# DT Displays -------------------------------------------------------------
    
    # Display // Events data table
    output$sdd_dt_events <- renderDT({
      
      # If no errors to get the data frame from mongoDB
      if (!inherits(events(), "try-error") && nrow(events()) > 0) {
        # The columns selection is now rendered by DT!
        events()
      } else {
        NULL
      }
    },
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 100,
      lengthMenu = c(50,100,200,500),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons    = I('colvis'))
    )
    
    # Display // Courses datatable
    output$sdd_dt_courses <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(courses(), "try-error") && nrow(courses()) > 0) {
        # Preparing the links
        courses <- prepare_url_alr_url(courses())
        # The columns selection is now rendered by DT !
        return(courses)
      } else {
        return(NULL)
      }
    },
    # To activate the link
    escape = FALSE,
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 50,
      lengthMenu = c(20,50,100),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons    = I('colvis'))
    )
    
    # Display // Modules datatable
    output$sdd_dt_modules <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(modules(), "try-error") && nrow(modules()) > 0) {
        # Preparing the links
        modules <- prepare_url_alr_url(modules())
        # The columns selection is now rendered by DT !
        return(modules)
      } else {
        return(NULL)
      }
    },
    # To activate the link
    escape = FALSE,
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 50,
      lengthMenu = c(20,50,100),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons   = I('colvis'))
    )
    
    # Display // Apps datatable
    output$sdd_dt_apps <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(apps(), "try-error") && nrow(apps()) > 0) {
        # Preparing the links
        apps <- prepare_url_alr_url(apps())
        # The columns selection is now rendered by DT !
        return(apps)
      } else {
        return(NULL)
      }
    },
    # To activate the link
    escape = FALSE,
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 50,
      lengthMenu = c(20,50,100),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons    = I('colvis'))
    )
    
    # Display // Planning datatable
    output$sdd_dt_planning <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(planning(), "try-error") && nrow(planning()) > 0) {
        # Preparing the links
        planning <- prepare_url_alr_url(planning())
        # The columns selection is now rendered by DT !
        return(planning)
      } else {
        return(NULL)
      }
    },
    # To activate the link
    escape = FALSE,
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 50,
      lengthMenu = c(20,50,100),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons    = I('colvis'))
    )
    
    # Display // Users2 datatable
    output$sdd_dt_users2 <- renderDT({
      
      # If no errors to get the dataframe from mongoDB
      if (!inherits(users2(), "try-error") && nrow(users2()) > 0) {
        # Preparing the links
        users2 <- prepare_url_alr_url(users2())
        # The columns selection is now rendered by DT !
        return(users2)
      } else {
        return(NULL)
      }
    },
    # To activate the link
    escape = FALSE,
    # Extension to display a button that allow col selection
    extensions = 'Buttons',
    # Options for the data table
    options = list(
      scrollX    = TRUE,
      pageLength = 50,
      lengthMenu = c(20,50,100),
      # Options to get the col selector and lengthmenu
      dom        = 'Blfrtip',
      buttons    = I('colvis'))
    )

# Communication -----------------------------------------------------------

    # Variable: all of module's vars
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
