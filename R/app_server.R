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
  mod_home_page_server("home_page_1", all_vars = all_vars)
  # Server module of 2nd page
  std_progression_vars <- mod_std_progression_server("std_progression_1", all_vars = all_vars)
  # Server module of 3rd page
  cls_progression_vars <- mod_cls_progression_server("cls_progression_1", all_vars = all_vars)
  # Server module of 4th page
  sdd_tables_vars <- mod_sdd_tables_server("sdd_tables_1", all_vars = all_vars)
  
  # Display the menuitems
  output$menuitems <- renderMenu({
    # New elements from tables_news
    new_elements <- nrow(req(right_sidebar_vars$h5p_news)) + nrow(req(right_sidebar_vars$learnr_news)) + nrow(req(right_sidebar_vars$shiny_news))
    
    # Tabs of the sidebar
    tagList(
      sidebarMenu(
        # First tab
        menuItem("Home Page", tabName = "home_page", icon = shiny::icon("home", verify_fa = FALSE), badgeLabel = boxLabel("News", status = "success")),
        # Second tab
        menuItem("Students Progression", tabName = "std_progression", icon = shiny::icon("graduation-cap", verify_fa = FALSE)),
        # Third tab
        menuItem("Courses Progression", tabName = "cls_progression", icon = shiny::icon("school", verify_fa = FALSE)),
        # Forth tab
        # If there are news, put a badge
        if (!is.null(new_elements) && new_elements > 0) {
          menuItem("Raw Data Exploration", tabName = "rawdatatable", icon = shiny::icon("table", verify_fa = FALSE), badgeLabel = new_elements, badgeColor = "red")
        } else {
          menuItem("Raw Data Exploration", tabName = "rawdatatable", icon = shiny::icon("table", verify_fa = FALSE))
        }
      )
    )
  })
}
