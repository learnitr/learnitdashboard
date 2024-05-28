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
    sdd_tables_vars = NULL,
  )
  
  # Updating the modules vars
  observe({
    all_vars$right_sidebar_vars <- right_sidebar_vars
  })
  observe({
    all_vars$sdd_tables_vars <- sdd_tables_vars
  })
  
  # Server module of the right sidebar for selectors
  right_sidebar_vars <- mod_right_sidebar_server("right_sidebar_1",
    all_vars = all_vars)
  # Server module of 1st page
  mod_home_page_server("home_page_1", all_vars = all_vars)
  # Server module of 2nd page
  mod_timelines_server("timelines_1", all_vars = all_vars)
  # Server module of 3rd page
  mod_progressions_server("progressions_1", all_vars = all_vars)
  # Server module of 4th page
  sdd_tables_vars <- mod_sdd_tables_server("sdd_tables_1", all_vars = all_vars)
  
  # Render loadingstate
  output$load <- renderUI({
    if (is.null(right_sidebar_vars$events_news)) {
      tagList(
        column( width = 4, offset = 4,
          box(headerBorder = FALSE, id = "loadstate", loadingState())
        )
      )
    }
  })
  
  # Display the menuitems
  output$menuitems <- renderMenu({
    # New elements from tables_news
    events_news <- req(right_sidebar_vars$events_news)
    new_apps <- req(right_sidebar_vars$apps_news)
    
    # Tabs of the sidebar
    tagList(
      sidebarMenu(
        # First tab
        menuItem("Home Page", tabName = "home_page",
          icon = shiny::icon("home", verify_fa = FALSE)),
        # Second tab
        # If there are news, put a badge
        if (!is.null(new_apps) && new_apps > 0) {
          menuItem("Timelines", tabName = "timelines",
            icon = shiny::icon("calendar", verify_fa = FALSE),
            badgeLabel = new_apps, badgeColor = "green")
        } else {
          menuItem("Timelines", tabName = "timelines",
            icon = shiny::icon("calendar", verify_fa = FALSE))
        },
        # Third tab
        menuItem("Progressions", tabName = "progressions",
          icon = shiny::icon("bars-progress", verify_fa = FALSE)),
        # Fourth tab
        # If there are news, put a badge
        if (!is.null(events_news) && events_news > 0) {
          menuItem("Raw Data Exploration", tabName = "rawdatatable",
            icon = shiny::icon("table", verify_fa = FALSE),
            badgeLabel = events_news, badgeColor = "red")
        } else {
          menuItem("Raw Data Exploration", tabName = "rawdatatable",
            icon = shiny::icon("table", verify_fa = FALSE))
        }
      )
    )
  })
}
