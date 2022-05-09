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
      dashboardHeader(
        title = "LearnItDashboard"
      ),
      
      # Sidebar of Dashboard
      dashboardSidebar(
        
        # Creation of the sidebar
        sidebarMenu(
          # First tab
          menuItem("SDD Dashboard", tabName = "mongodbsdd"),
          # Second tab
          menuItem("Building", tabName = "building")
        )
        
      ),
      
      # Body of Dashboard
      dashboardBody(
        tabItems(
          
          # First tab
          tabItem(tabName = "mongodbsdd",
            # Page title
            h1("SDD Dashboard"),
            # Page Module
            mod_test_1_ui("test_1_1"),
          ),
          
          # Second tab (test)
          tabItem(tabName = "building",
            h1("Nothing yet...")
          )
          
        ),
      ),
      
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
