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
    
    # UIoutput for the apps timeline
    uiOutput(ns("ui_apps_timeline")),
    
    # UIoutput for the planning timeline
    uiOutput(ns("ui_planning_timeline"))
  )
}
    
#' timeslines Server Functions
#'
#' @noRd 
mod_timeslines_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------
    
# Global Vars -------------------------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to users
    sdd_apps <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)
    sdd_planning <- try(mongolite::mongo("planning", url = sdd_url), silent = TRUE)

# Apps Timeline -----------------------------------------------------------

    # Display the output inside the UI if there is no error to get the apps databases
    output$ui_apps_timeline <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Apps Timeline (grouped by icourse)", solidHeader = TRUE,
            width = 12, icon = shiny::icon("calendar", verify_fa = FALSE),
            collapsible = TRUE, label = boxLabel(2, "danger"), status = "purple",
            # Sidebar for timeline options
            sidebar = boxSidebar(
              id = ns("apps_timeline_sb"),
              width = 25,
              tagList(
                tags$br(),
                tags$h4("Navigation :"),
                actionButton(ns("apps_center_to_actual_week"), "Center to current Week"),
                tags$br(),
                tags$br(),
                selectInput(ns("apps_center_to_month"), NULL, choices = c("Center to Month","September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August"), width = "90%"),
                selectInput(ns("selected_app"), NULL, choices = c("Center to App", sdd_apps$distinct("app")), width = "90%")
              )
            ),
            timevisOutput(ns("apps_timeline"))
          )
        )
      }
    })
    
    # Updating the boxsidebar to make it available
    updateBoxSidebar("apps_timeline_sb", session = session)
    
    # Variable : Data of the timeline
    timeline_data <- NULL
    apps_data <- try(na.omit(sdd_apps$find('{}', fields = '{"app" : true, "start" : true ,"end" : true, "icourse" : true, "type" : true, "url" : true, "alt_url" : true}')), silent = TRUE)
    planning_data <- try(na.omit(sdd_planning$find('{}', fields = '{"label" : true, "start" : true, "end" : true, "url" : true, "alt_url" : true, "summary" : true, "icourse" : true}')), silent = TRUE)
    
    # Preparing the timeline_data
    if (!inherits(apps_data, "try-error") && !inherits(planning_data, "try-error")) {
      
      # Preparing app dataframe's content (with url and else)
      apps_data$content <- try(prepare_content(apps_data))
      apps_data$title <- NA
      apps_data$style <- try(prepare_style(apps_data))
      # Preparing app dataframe's content (with url and else) and group and app
      planning_data$content <- try(prepare_content(planning_data))
      planning_data$group <- "Classes"
      planning_data$app <- NA
      planning_data$style <- "background-color : #F4A460; font-weight : bold;"
      
      if (!is.null(apps_data$content) && !is.null(planning_data$content)) {
        # Taking only interesting columns
        apps_data <- apps_data[apps_data$start < apps_data$end ,c("_id", "content", "icourse", "start", "end", "app", "title", "style")]
        planning_data <- planning_data[c("_id", "content", "group", "start", "end", "app", "summary", "style")]
        # Setting the good names to fit timevis
        names(apps_data) <- c("id", "content", "group", "start", "end", "app", "title", "style")
        names(planning_data) <- c("id", "content", "group", "start", "end", "app", "title", "style")
        
        # Binding apps and planning dataframes
        timeline_data <- rbind(apps_data, planning_data)
        
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
        # Creation of start date for the initial window of the timeline
        start <- lubridate::floor_date(Sys.time(), "week")
        lubridate::day(start) <- lubridate::day(start) + 1
        # Creation of end date for the initial window of the timeline
        end <- lubridate::ceiling_date(Sys.time(), "week")
        lubridate::day(end) <- lubridate::day(end) + 1
        config <- list(
          start = start,
          end = end
        )
        return(timevis(timeline_data, groups = attr(timeline_data, "groups"), options = config))
      }
    })

# Planning Timeline -------------------------------------------------------

    # Display the output inside the UI if there is no error to get the apps databases
    output$ui_planning_timeline <- renderUI({
      if (!inherits(sdd_planning, "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Planning Timeline (grouped by icourse)", solidHeader = TRUE,
               width = 12, icon = shiny::icon("calendar", verify_fa = FALSE),
               collapsible = TRUE, label = boxLabel(3, "danger"), status = "purple",
               # Sidebar for timeline options
               sidebar = boxSidebar(
                 id = ns("planning_timeline_sb"),
                 width = 25,
                 tagList(
                   tags$br(),
                   tags$h4("Navigation :"),
                   actionButton(ns("plan_center_to_actual_week"), "Center to current Week"),
                   tags$br(),
                   tags$br(),
                   selectInput(ns("plan_center_to_month"), NULL, choices = c("Center to Month","September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August"), width = "90%"),
                   selectInput(ns("selected_label"), NULL, choices = c("Center to Label", sdd_planning$distinct("label")), width = "90%")
                 )
               ),
               timevisOutput(ns("planning_timeline"))
          )
        )
      }
    })
    
    # Updating the boxsidebar to make it available
    updateBoxSidebar("planning_timeline_sb", session = session)
    
    # Variable : Data of the timeline
    planning_timeline_data <- NULL
    # Getting the planning data
    planning_timeline_data <- try(na.omit(sdd_planning$find('{}', fields = '{"label" : true, "start" : true, "end" : true, "url" : true, "alt_url" : true, "summary" : true, "icourse" : true}')), silent = TRUE)
    
    if (!inherits(planning_timeline_data, "try-error")) {
      # Preparing the content for the timeline
      planning_timeline_data$content <- try(prepare_content(planning_timeline_data))
      
      if (!is.null(planning_timeline_data$content)) {
        # Taking the good columns
        planning_timeline_data <- planning_timeline_data[c("_id", "content", "icourse", "start", "end", "summary", "label")]
        # Setting the good names for columns for timevis
        names(planning_timeline_data) <- c("id", "content", "group", "start", "end", "title", "label")
        # Getting the groups
        attr(planning_timeline_data, "groups") <- data.frame(
          id = unique(planning_timeline_data$group),
          content = unique(planning_timeline_data$group)
        )
      } else { planning_timeline_data <- NULL }
    } else { planning_timeline_data <- NULL }
    
    # Display the timeline when the output is available and when the data is available
    output$planning_timeline <- renderTimevis({
      if (!is.null(planning_timeline_data)) {
        # Creation of start date for the initial window of the timeline
        start <- lubridate::floor_date(Sys.time(), "week")
        lubridate::day(start) <- lubridate::day(start) + 1
        # Creation of end date for the initial window of the timeline
        end <- lubridate::ceiling_date(Sys.time(), "week")
        lubridate::day(end) <- lubridate::day(end) + 1
        config <- list(
          start = start,
          end = end
        )
        return(timevis(planning_timeline_data, groups = attr(planning_timeline_data, "groups"), options = config))
      }
    })
    
# Timeline Update ---------------------------------------------------------

    # Update Apps - Changing the timevis window
    observeEvent(input$selected_app, {
      if (input$selected_app != "Center to App") {
        # Get the dates of start and end of selected app
        start <- as.POSIXct(na.omit(timeline_data[timeline_data$app == input$selected_app,"start"]))
        end <- as.POSIXct(na.omit(timeline_data[timeline_data$app == input$selected_app,"end"]))
        # Change dates to have a range (7 days before the start <-> 7 days after the end)
        lubridate::day(start) <- lubridate::day(start) - 7
        lubridate::day(end) <- lubridate::day(end) + 7
        # Set the window on the range of dates
        setWindow("apps_timeline", start = start, end = end)
      }
    })
    
    # Update Planning - Changing the timevis window
    observeEvent(input$selected_label, {
      if (input$selected_label != "Center to Label") {
        # Get the dates of start and end of selected label
        start <- as.POSIXct(na.omit(planning_timeline_data[planning_timeline_data$label == input$selected_label,"start"]))
        end <- as.POSIXct(na.omit(planning_timeline_data[planning_timeline_data$label == input$selected_label,"end"]))
        # Change dates to have a range the day to the next
        start <- lubridate::floor_date(start, "day")
        lubridate::day(end) <- lubridate::day(end) + 1
        # Set the window on the range of dates
        setWindow("planning_timeline", start = start, end = end)
      }
    })
    
    # Update Apps - Getting back to the current week
    observeEvent(input$apps_center_to_actual_week, {
      # Creation of start date of the week
      start <- lubridate::floor_date(Sys.time(), "week")
      lubridate::day(start) <- lubridate::day(start) + 1
      # Creation of end date of the week
      end <- lubridate::ceiling_date(Sys.time(), "week")
      lubridate::day(end) <- lubridate::day(end) + 1
      setWindow("apps_timeline", start = start, end = end)
    })
    
    # Update Planning - Getting back to the current week 
    observeEvent(input$plan_center_to_actual_week, {
      # Creation of start date of the week
      start <- lubridate::floor_date(Sys.time(), "week")
      lubridate::day(start) <- lubridate::day(start) + 1
      # Creation of end date of the week
      end <- lubridate::ceiling_date(Sys.time(), "week")
      lubridate::day(end) <- lubridate::day(end) + 1
      setWindow("planning_timeline", start = start, end = end)
    })
    
    # Update Apps - Getting back to the current week
    observeEvent(input$apps_center_to_month, {
      if (input$apps_center_to_month != "Center to Month") {
        # Getting current time
        time <- Sys.Date()
        # Getting the month
        sel_month <- switch (input$apps_center_to_month,
          "September" = 9,
          "October" = 10,
          "November" = 11,
          "December" = 12,
          "January" = 1,
          "Februrary" = 2,
          "March" = 3,
          "April" = 4,
          "May" = 5,
          "June" = 6,
          "July" = 7,
          "August" = 8
        )
        # Setting the good year
        if (sel_month > 8) {
          lubridate::year(time) <- lubridate::year(time) - 1
        }
        # Setting the selected month
        lubridate::month(time) <- sel_month
        # Getting start day of month and end day
        start <- lubridate::floor_date(time, "month")
        end <- lubridate::ceiling_date(time, "month") - 1
        setWindow("apps_timeline", start = start, end = end)
      }
    })
    
    # Update Planning - Getting back to the current week
    observeEvent(input$plan_center_to_month, {
      if (input$plan_center_to_month != "Center to Month") {
        # Getting current time
        time <- Sys.Date()
        # Getting the month
        sel_month <- switch (input$plan_center_to_month,
                             "September" = 9,
                             "October" = 10,
                             "November" = 11,
                             "December" = 12,
                             "January" = 1,
                             "Februrary" = 2,
                             "March" = 3,
                             "April" = 4,
                             "May" = 5,
                             "June" = 6,
                             "July" = 7,
                             "August" = 8
        )
        # Setting the good year
        if (sel_month > 8) {
          lubridate::year(time) <- lubridate::year(time) - 1
        }
        # Setting the selected month
        lubridate::month(time) <- sel_month
        # Getting start day of month and end day
        start <- lubridate::floor_date(time, "month")
        end <- lubridate::ceiling_date(time, "month") - 1
        setWindow("planning_timeline", start = start, end = end)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_timeslines_ui("timeslines_1")
    
## To be copied in the server
# mod_timeslines_server("timeslines_1")
