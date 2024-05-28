# TODO: use dashboardthemes::shinyDashboardThemes()?

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
    dashboardPage(
      
      # Head of Dashboard
      header = dashboardHeader(
        controlbarIcon = shiny::icon("gears", verify_fa = FALSE),
        title = tagList(
          span(class = "logo-lg", "LearnItDashboard"), 
          img(
            src = "https://avatars.githubusercontent.com/u/11614296?s=280&v=4",
            style = "width: 35px"
          )
        )
      ),
      
      # Sidebar of Dashboard
      sidebar = dashboardSidebar(
        # Sidebar dynamically generated in server
        sidebarMenuOutput("menuitems")
      ),
      
      # Body of Dashboard
      body = dashboardBody(
        useShinyjs(),
        
        uiOutput("load"),
        
        tabItems(
          
          # First tab : Home Page with general informations
          tabItem(tabName = "home_page",
            # Page Module
            mod_home_page_ui("home_page_1")
          ),
          
          # Second tab : Timelines
          tabItem(tabName = "timelines",
            # Page Module
            mod_timelines_ui("timeslines_1")
          ),
          
          # Third tab : Progressions
          tabItem(tabName = "progressions",
            # Page Module
            mod_progressions_ui("progressions_1")
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
        overlay = FALSE,
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
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "learnitdashboard"
    ),
    tags$style(
      "@import url(https://use.fontawesome.com/releases/v6.1.2/css/all.css);"),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
