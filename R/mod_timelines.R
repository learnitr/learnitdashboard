#' timelines UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_timelines_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # UIoutput for the apps timeline
    uiOutput(ns("ui_apps_timeline")),
    
    # UIoutput for the planning timeline
    uiOutput(ns("ui_planning_timeline"))
  )
}
    
#' timelines Server Functions
#'
#' @noRd 
mod_timelines_server <- function(id, all_vars) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------
    
    # Vars from rightsidebar
    apps <- reactive({all_vars$right_sidebar_vars$apps})
    planning <- reactive({all_vars$right_sidebar_vars$planning})
    
# Global Vars -------------------------------------------------------------

# Apps Timeline -----------------------------------------------------------

    # Display the output inside the UI if no error to get the apps databases
    output$ui_apps_timeline <- renderUI({
      if (!inherits(apps(), "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Apps Timeline (grouped by icourse)", solidHeader = TRUE,
            width = 12, icon = shiny::icon("calendar", verify_fa = FALSE),
            collapsible = TRUE, status = "info",
            # Sidebar for timeline options
            sidebar = boxSidebar(
              id = ns("apps_timeline_sb"),
              width = 25,
              tagList(
                tags$br(),
                tags$h4("Navigation :"),
                actionButton(ns("apps_center_to_actual_week"),
                  "Center to current Week"),
                tags$br(),
                tags$br(),
                selectInput(ns("apps_center_to_month"), NULL,
                  choices = c("Center to Month",
                    "September", "October", "November", "December", "January",
                    "February", "March", "April", "May", "June", "July",
                    "August"), width = "90%"),
                selectInput(ns("selected_app"), NULL,
                  choices = c("Center to App", sort(unique(apps()$app))),
                  width = "90%")
              )
            ),
            timevisOutput(ns("apps_timeline"))
          )
        )
      }
    })
    
    # Updating the boxsidebar to make it available
    updateBoxSidebar("apps_timeline_sb", session = session)
    
    # Variable Data of the timeline
    apps_timeline_data <- reactive({
      if (!inherits(apps(), "try-error") && nrow(apps()) > 0 &&
        !inherits(planning(), "try-error") && nrow(planning()) > 0) {
        
        # Setting the apps and planning data
        apps_data <- na.omit(apps()[c("app", "start", "end", "icourse", "type",
          "url", "alt_url")])
        planning_data <- na.omit(planning()[c("label", "start", "end",
          "url", "alt_url", "summary", "icourse")])
        
        if (nrow(apps_data) > 0 && nrow(planning_data) > 0) {
          # Preparing apps data frame
          apps_data$content <- prepare_content(apps_data)
          apps_data$style <- prepare_style(apps_data)
          apps_data$title <- NA
          apps_data$id <- 1:nrow(apps_data)
          # Preparing planning data frame
          planning_data$content <- prepare_content(planning_data)
          planning_data$style <-
            "background-color : #F4A460; font-weight : bold;"
          planning_data$group <- NA # "Classes"
          planning_data$app <- NA
          planning_data$id <- (1 + nrow(apps_data)):(nrow(apps_data) +
            nrow(planning_data))
          
          # Taking only interesting columns
          apps_data <- apps_data[apps_data$start < apps_data$end, c("id",
            "content", "icourse", "start", "end", "app", "title", "style")]
          planning_data <- planning_data[c("id", "content", "group", "start",
            "end", "app", "summary", "style")]
          # Setting the good names to fit timevis
          names(apps_data) <- c("id", "content", "group", "start", "end",
            "app", "title", "style")
          names(planning_data) <- c("id", "content", "group", "start", "end",
            "app", "title", "style")
          # Adding style and type to data
          apps_data$type <- NA
          planning_data$type <- "background"
          
          # Creating the result data frame
          timeline_df <- rbind(apps_data, planning_data)
          
          # Preparing the groups for timevis
          attr(timeline_df, "groups") <- data.frame(
            id = unique(na.omit(timeline_df$group)),
            content = unique(na.omit(timeline_df$group))
          )
          
          return(timeline_df)
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    
    # Display the timeline when the output and the data are available
    output$apps_timeline <- renderTimevis({
      if (!is.null(apps_timeline_data())) {
        # Creation of start date for the initial window of the timeline
        start <- lubridate::floor_date(Sys.time(), "week")
        lubridate::day(start) <- lubridate::day(start) + 1
        # Creation of end date for the initial window of the timeline
        end <- lubridate::ceiling_date(Sys.time(), "week")
        lubridate::day(end) <- lubridate::day(end) + 1
        config <- list(start = start, end = end)
        return(timevis(apps_timeline_data(),
          groups = attr(apps_timeline_data(), "groups"), options = config))
      }
    })

# Planning Timeline -------------------------------------------------------

    # Display the output inside the UI if no error to get the apps databases
    output$ui_planning_timeline <- renderUI({
      if (!inherits(planning(), "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Planning Timeline (grouped by icourse)",
            solidHeader = TRUE, width = 12,
            icon = shiny::icon("calendar", verify_fa = FALSE),
              collapsible = TRUE, status = "info",
              # Sidebar for timeline options
              sidebar = boxSidebar(
                id = ns("planning_timeline_sb"),
                width = 25,
                tagList(
                  tags$br(),
                  tags$h4("Navigation :"),
                  actionButton(ns("plan_center_to_actual_week"),
                    "Center to current Week"),
                  tags$br(),
                  tags$br(),
                  selectInput(ns("plan_center_to_month"), NULL,
                    choices = c("Center to Month",
                      "September", "October", "November", "December", "January",
                      "February", "March", "April", "May", "June", "July",
                      "August"), width = "90%"),
                  selectInput(ns("selected_label"), NULL,
                    choices = c("Center to Module",
                      sort(unique(planning()$label))), width = "90%")
                )
              ),
              timevisOutput(ns("planning_timeline")
            )
          )
        )
      }
    })
    
    # Updating the boxsidebar to make it available
    updateBoxSidebar("planning_timeline_sb", session = session)
    
    # Variable : Data of the timeline
    planning_timeline_data <- reactive({
      if (!inherits(planning(), "try-error") && nrow(planning()) > 0) {

        planning_df <- na.omit(planning()[c("label", "start", "end",
          "url", "alt_url", "summary", "icourse")])

        if (nrow(planning_df) > 0) {
          # Preparing planning data frame
          planning_df$content <- prepare_content(planning_df)
          planning_df$id <- 1:(nrow(planning_df))
  
          # Taking the good columns
          planning_df <- planning_df[c("id", "content", "icourse", "start",
            "end", "summary", "label")]
          # Setting the good names for columns for timevis
          names(planning_df) <- c("id", "content", "group", "start",
            "end", "title", "label")
          
          # Getting the groups
          attr(planning_df, "groups") <- data.frame(
            id      = unique(planning_df$group),
            content = unique(planning_df$group)
          )
          
          return(planning_df)
        } else {
          NULL
        }
      } else {
        NULL
      }
    })
    
    # Display the timeline when the output and the data are available
    output$planning_timeline <- renderTimevis({
      if (!is.null(planning_timeline_data())) {
        # Creation of start date for the initial window of the timeline
        start <- lubridate::floor_date(Sys.time(), "week")
        lubridate::day(start) <- lubridate::day(start) + 1
        # Creation of end date for the initial window of the timeline
        end <- lubridate::ceiling_date(Sys.time(), "week")
        lubridate::day(end) <- lubridate::day(end) + 1
        config <- list(start = start, end = end)
        return(timevis(planning_timeline_data(),
          groups = attr(planning_timeline_data(), "groups"), options = config))
      }
    })
    
# Timeline Update ---------------------------------------------------------

    # Update Apps - Changing the timevis window
    observeEvent(input$selected_app, {
      if (input$selected_app != "Center to App") {
        # Get the dates of start and end of selected app
        start <- as.POSIXct(na.omit(apps_timeline_data()[
          apps_timeline_data()$app == input$selected_app,"start"]))
        end <- as.POSIXct(na.omit(apps_timeline_data()[
          apps_timeline_data()$app == input$selected_app,"end"]))
        # Change dates to have a range (7 days before the start <-> 7 days after the end)
        lubridate::day(start) <- lubridate::day(start) - 7
        lubridate::day(end) <- lubridate::day(end) + 7
        # Set the window on the range of dates
        setWindow("apps_timeline", start = start, end = end)
      }
    })
    
    # Update Planning - Changing the timevis window
    observeEvent(input$selected_label, {
      if (input$selected_label != "Center to Module") {
        # Get the dates of start and end of selected label
        start <- as.POSIXct(na.omit(planning_timeline_data()[
          planning_timeline_data()$label == input$selected_label,"start"]))
        end <- as.POSIXct(na.omit(planning_timeline_data()[
          planning_timeline_data()$label == input$selected_label,"end"]))
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
        sel_month <- switch(input$apps_center_to_month,
          "September" = 9,
          "October"   = 10,
          "November"  = 11,
          "December"  = 12,
          "January"   = 1,
          "February"  = 2,
          "March"     = 3,
          "April"     = 4,
          "May"       = 5,
          "June"      = 6,
          "July"      = 7,
          "August"    = 8
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
        sel_month <- switch(input$plan_center_to_month,
          "September" = 9,
          "October"   = 10,
          "November"  = 11,
          "December"  = 12,
          "January"   = 1,
          "February"  = 2,
          "March"     = 3,
          "April"     = 4,
          "May"       = 5,
          "June"      = 6,
          "July"      = 7,
          "August"    = 8
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
# mod_timelines_ui("timelines_1")
    
## To be copied in the server
# mod_timeslines_server("timelines_1")
