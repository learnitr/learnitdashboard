#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage( skin = "purple",
      
      # Head of Dashboard
      header = dashboardHeader(
        controlbarIcon = shiny::icon("gears", verify_fa = FALSE),
        title = h3("LearnItDashboard")
      ),
      
      # Sidebar of Dashboard
      sidebar = dashboardSidebar(
        
        # Creation of the sidebar
        sidebarMenu(
          # First tab
          menuItem("Home Page", tabName = "home_page", icon = shiny::icon("home", verify_fa = FALSE)),
          # Second tab
          menuItem("Students Progression", tabName = "std_progression", icon = shiny::icon("graduation-cap", verify_fa = FALSE)),
          # Third tab
          menuItem("Courses Progression", tabName = "cls_progression", icon = shiny::icon("school", verify_fa = FALSE)),
          # Forth tab
          menuItem("Raw Data Exploration", tabName = "rawdatatable", icon = shiny::icon("table", verify_fa = FALSE))
        )
        
      ),
      
      # Body of Dashboard
      body = dashboardBody(
        useShinyjs(),
        tabItems(
          
          # First tab : Home Page with general informations
          tabItem(tabName = "home_page",
            # Page Module
            mod_home_page_ui("home_page_1")
          ),
          
          # Second tab : Student progression
          tabItem(tabName = "std_progression",
            h1("Students Progression"),
            # Page Module
            mod_std_progression_ui("std_progression_1")
          ),
          
          # Third tab : Course progression
          tabItem(tabName = "cls_progression",
            h1("Courses Progression"),
            # Page Module
            mod_cls_progression_ui("cls_progression_1")
          ),
          
          # Forth tab : Data Table Exploration
          tabItem(tabName = "rawdatatable",
            # Page Module
            mod_sdd_tables_ui("sdd_tables_1")
          )
        )
      ),
      
      # Right sidebar with module : selectors and request maker
      controlbar = dashboardControlbar(
        collapsed = FALSE,
        skin = "dark",
        mod_right_sidebar_ui("right_sidebar_1")
      )
      
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "learnitdashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
