#' timeslines UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timeslines_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # UIoutput for the timeline
    uiOutput(ns("ui_apps_timeline")),
  )
}
    
#' timeslines Server Functions
#'
#' @noRd 
mod_timeslines_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------
    
    selected_app <- reactive({all_vars$right_sidebar_vars$selected_app})
    
# Global Vars -------------------------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to users
    sdd_apps <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)

# Apps Timeline -----------------------------------------------------------

    # Display the output inside the UI if there is no error to get the apps databases
    output$ui_apps_timeline <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Apps Timeline (grouped by icourse)", solidHeader = TRUE,
               width = 12, icon = shiny::icon("calendar", verify_fa = FALSE), collapsible = TRUE,
               label = boxLabel(2, "danger"), collapsed = TRUE, status = "purple",
               timevisOutput(ns("apps_timeline"))
          )
        )
      }
    })
    
    # Variable : Data of the second timeline, to make groups by icourse
    timeline_data <- try(na.omit(sdd_apps$find('{}', fields = '{"app" : true, "start" : true ,"end" : true, "icourse" : true, "url" : true, "alt_url" : true}')), silent = TRUE)
    # Preparing the timeline_data
    if (!inherits(timeline_data, "try-error")) {
      
      # Preparing the content with a link (url from app)
      timeline_data$content <- try(html_link_with_app_name(timeline_data))
      
      if (!is.null(timeline_data$content)) {
        timeline_data <- timeline_data[timeline_data$start < timeline_data$end ,c("_id", "content", "icourse", "start", "end", "app")]
        # Setting the good names to fit timevis
        names(timeline_data) <- c("id", "content", "group", "start", "end", "app")
        
        # Preparing the groups for timevis
        attr(timeline_data, "groups") <- data.frame(
          id = unique(timeline_data$group),
          content = unique(timeline_data$group)
        )
      } else { timeline_data <- NULL }
    } else { timeline_data <- NULL }
    
    # Display the timeline when the output is available and when the data is available
    output$apps_timeline <- renderTimevis({
      if (!is.null(timeline_data)) {
        start <- lubridate::floor_date(Sys.time(), "week")
        lubridate::day(start) <- lubridate::day(start) + 1
        end <- lubridate::ceiling_date(Sys.time(), "week")
        lubridate::day(end) <- lubridate::day(end) + 1
        config <- list(
          start = start,
          end = end
        )
        return(timevis(timeline_data, groups = attr(timeline_data, "groups"), options = config))
      }
    })

# Timeline Update ---------------------------------------------------------

    # Update - Changing the timevis window
    observeEvent(selected_app(), {
      if (selected_app() != "All") {
        # Get the dates of start and end of selected app
        start <- as.POSIXct(timeline_data[timeline_data$app == selected_app(),"start"])
        end <- as.POSIXct(timeline_data[timeline_data$app == selected_app(),"end"])
        # Change dates to have a range (7 days before the start <-> 7 days after the end)
        lubridate::day(start) <- lubridate::day(start) - 7
        lubridate::day(end) <- lubridate::day(end) + 7
        # Set the window on the range of dates
        setWindow("apps_timeline", start = start, end = end)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_timeslines_ui("timeslines_1")
    
## To be copied in the server
# mod_timeslines_server("timeslines_1")
