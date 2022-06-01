#' home_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_home_page_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    # Global Informations Boxes
    fluidRow(
      infoBoxOutput(ns("infobox_1")),
      infoBoxOutput(ns("infobox_2")),
      infoBoxOutput(ns("infobox_3")),
      
      infoBoxOutput(ns("infobox_4")),
      infoBoxOutput(ns("infobox_5")),
      infoBoxOutput(ns("infobox_6")),
      
      # valueBoxOutput(ns("valuebox_1")),
    ),
    
    # UIoutput to generate the timelines of differents apps
    uiOutput(ns("ui_apps_timelines")),
    
    # UIoutput to generate a timeline of the different apps
    uiOutput(ns("ui_apps_timeline_1")),
    # test 2
    uiOutput(ns("ui_apps_timeline_2")),
    
    # UIoutput to generate a box and a plot inside (of students per courses)
    uiOutput(ns("courses_students_plot"))
  )
}
    
#' home_page Server Functions
#'
#' @noRd 
mod_home_page_server <- function(id, all_vars){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# Getting Modules Vars ----------------------------------------------------

    # Vars from right_sidebar
    h5p_news <- reactive({all_vars$right_sidebar_vars$h5p_news})
    learnr_news <- reactive({all_vars$right_sidebar_vars$learnr_news})
    shiny_news <- reactive({all_vars$right_sidebar_vars$shiny_news})
    selected_news_time <- reactive({all_vars$right_sidebar_vars$selected_news_time})
    selected_app <- reactive({all_vars$right_sidebar_vars$selected_app})
    
# Global Vars -------------------------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to users
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    sdd_apps <- try(mongolite::mongo("apps", url = sdd_url), silent = TRUE)
    
# Display Boxes -----------------------------------------------------------

    # Display // Info Box 1
    output$infobox_1 <- renderInfoBox({
      
      # The boxes need to have something inside, even empty, so it tests if we can put our data inside
      if (!inherits(h5p_news(), "try-error")) {
        infoBox(
          title = "H5P",
          subtitle = paste0("New entries since : ", selected_news_time()),
          # Show the amount of changes if there are some
          value = if (nrow(h5p_news()) > 0) {
            nrow(h5p_news())
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # Display // Info Box 2
    output$infobox_2 <- renderInfoBox({
      
      if (!inherits(learnr_news(), "try-error")) {
        infoBox(
          title = "Learnr",
          subtitle = paste0("New entries since : ", selected_news_time()),
          # Show the amount of changes if there are some
          value = if (nrow(learnr_news()) > 0) {
            nrow(learnr_news())
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # Display // Info Box 3
    output$infobox_3 <- renderInfoBox({
      
      if (!inherits(shiny_news(), "try-error")) {
        infoBox(
          title = "Shiny",
          subtitle = paste0("New entries since : ", selected_news_time()),
          # Show the amount of changes if there are some
          value = if (nrow(shiny_news()) > 0) {
            nrow(shiny_news())
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # Display // Info Box 4
    output$infobox_4 <- renderInfoBox({
      
      # The boxes need to have something inside, even empty, so it tests if we can put our data inside
      if (!inherits(h5p_news(), "try-error")) {
        # Getting h5p news apps
        h5p_news_apps <- sort(unique(h5p_news()$app))
        infoBox(
          title = "H5P",
          # Show the apps if there are
          subtitle = if (!is.null(h5p_news_apps)) {
            selectInput(ns("h5p_apps_show"), NULL, choices = h5p_news_apps)
          } else {
            NULL
          },
          # Show the amoun of apps that changed and in how much courses
          value = if (nrow(h5p_news()) > 0) {
            paste0(length(unique(h5p_news()$app)), " apps changed in ", length(unique(h5p_news()$course)), " courses")
          } else { "No changes" },
          icon = icon("pencil", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("pencil", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # Display // Info Box 5
    output$infobox_5 <- renderInfoBox({
      
      if (!inherits(learnr_news(), "try-error")) {
        # Getting learnr news apps
        learnr_news_apps <- sort(unique(learnr_news()$app))
        infoBox(
          title = "Learnr",
          # Show the apps if there are
          subtitle = if (!is.null(learnr_news_apps)) {
            selectInput(ns("learnr_apps_show"), NULL, choices = learnr_news_apps)
          } else {
            NULL
          },
          # Show the amoun of apps that changed and in how much courses
          value = if (nrow(learnr_news()) > 0) {
            paste0(length(unique(learnr_news()$app)), " apps changed in ", length(unique(learnr_news()$course)), " courses")
          } else { "No changes" },
          icon = icon("chalkboard", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("chalkboard", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # Display // Info Box 6
    output$infobox_6 <- renderInfoBox({
      
      if (!inherits(shiny_news(), "try-error")) {
        # Getting shiny news apps
        shiny_news_apps <- sort(unique(shiny_news()$app))
        infoBox(
          title = "Shiny",
          # Show the apps if there are
          subtitle = if (!is.null(shiny_news_apps)) {
            selectInput(ns("shiny_apps_show"), NULL, choices = shiny_news_apps)
          } else {
            NULL
          },
          # Show the amoun of apps that changed and in how much courses
          value = if (nrow(shiny_news()) > 0) {
            paste0(length(unique(shiny_news()$app)), " apps changed in ", length(unique(shiny_news()$course)), " courses")
          } else { "No changes" },
          icon = icon("tablet", verify_fa = FALSE),
          color = "maroon"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("tablet", verify_fa = FALSE),
          color = "maroon"
        )
      }
    })
    
    # # Display // Value Box 1
    # output$valuebox_1 <- renderValueBox({
    #   
    #   if (!inherits(h5p_news(), "try-error")) {
    #     valueBox(
    #       value = paste0(nrow(h5p_news()), " entries"),
    #       subtitle = "H5P Apps",
    #       icon = icon("gears", verify_fa = FALSE),
    #       color = "maroon"
    #     )
    #   } else {
    #     valueBox(
    #       value = NULL,
    #       subtitle = "",
    #       icon = icon("gears", verify_fa = FALSE),
    #       color = "maroon"
    #     )
    #   }
    # })

# Apps Timelines ----------------------------------------------------------

    # Display the output of the tabbox
    output$ui_apps_timelines <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        tagList(
          tabBox( title = "Apps Timelines", width = 12, id = "selected_app_tab",
            # Display of the H5P timeline
            tabPanel("Ind. GitHub",
              timevisOutput(ns("igh_timeline"))
            ),
            # Display of the H5P timeline
            tabPanel("Group GitHub",
              timevisOutput(ns("ggh_timeline"))
            ),
            # Display of the H5P timeline
            tabPanel("H5P",
              timevisOutput(ns("h5p_timeline"))
            ),
            # Display of the Learnr timeline
            tabPanel("Learnr",
              timevisOutput(ns("learnr_timeline"))
            ),
            # Display of the Shiny timeline
            tabPanel("Shiny",
              timevisOutput(ns("shiny_timeline"))
            )
          )
        )
      }
    })

# 1 Apps Timeline (grouped by type) -----------------------------------------------------------

    # Variable : Courses from Apps table
    apps_courses <- try(sdd_apps$distinct("icourse"), silent = TRUE)
    
    # Display the output inside the UI if there is no error to get the apps databases
    output$ui_apps_timeline_1 <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Apps Timeline (grouped by type)", solidHeader = TRUE,
            width = 12, icon = shiny::icon("calendar", verify_fa = FALSE), collapsible = TRUE,
            label = boxLabel(1, "info"), collapsed = TRUE, status = "maroon",
            # Selector of course to make the timeline more clear
            selectInput(ns("at_selected_course"), NULL, choices = apps_courses),
            timevisOutput(ns("apps_timeline_1"))
          )
        )
      }
    })
    
    # Variable : Data for the timeline (apps depending on a selected course)
    timeline_data_1 <- reactive({
      req(input$at_selected_course)
      
      # Preparing the request depending on the selected course
      request <- glue::glue(r"--[{ "icourse" : "<<input$at_selected_course>>" }]--", .open = "<<", .close = ">>")
      # Making the request to database
      apps_datatable <- try(na.omit(sdd_apps$find(request, fields = '{"app" : true, "start" : true ,"end" : true, "type" : true, "url" : true, "alt_url" : true}')), silent = TRUE)
      
      if (!inherits(apps_datatable, "try-error")) {
        
        # Preparing the content with a link (url from app)
        apps_datatable$content <- try(html_link_with_app_name(apps_datatable))
        
        if (!is.null(apps_datatable$content)) {
          # Getting only the interesting columns (and throwing away the rows where start is after end)
          apps_datatable <- apps_datatable[apps_datatable$start < apps_datatable$end,c("_id", "content", "type", "start", "end", "app")]
          
          # Change the names to make them fit with timevis (_id = id, app = content, type = group, end = end, start = start)
          names(apps_datatable) <- c("id", "content", "group", "start", "end", "app")
          
          # Creating groups from the type
          timeline_data_1_groups <- data.frame(
            id = unique(apps_datatable$group),
            content = unique(apps_datatable$group)
          )
          
          # Attaching the groups to the datatable
          attr(apps_datatable, "groups") <- timeline_data_1_groups
          
          return(apps_datatable)
        } else { return (NULL) }
      # If error : NULL
      } else { return(NULL) }
    })
    
    # Display the timeline when the output is available and when the data is available
    output$apps_timeline_1 <- renderTimevis({
      if (!is.null(timeline_data_1())) {
        timevis(timeline_data_1(), groups = attr(timeline_data_1(), "groups"))
      }
    })

# 2 Apps Timeline (grouped by icourse) --------------------------------------

    # Display the output inside the UI if there is no error to get the apps databases
    output$ui_apps_timeline_2 <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        tagList(
          # Box in which the timeline will appear
          box( title = "Apps Timeline (grouped by icourse)", solidHeader = TRUE,
               width = 12, icon = shiny::icon("calendar", verify_fa = FALSE), collapsible = TRUE,
               label = boxLabel(2, "info"), collapsed = TRUE, status = "maroon",
               timevisOutput(ns("apps_timeline_2"))
          )
        )
      }
    })
    
    # Variable : Data of the second timeline, to make groups by icourse
    timeline_data_2 <- try(na.omit(sdd_apps$find('{}', fields = '{"app" : true, "start" : true ,"end" : true, "icourse" : true, "url" : true, "alt_url" : true}')), silent = TRUE)
    # Preparing the timeline_data_2
    if (!inherits(timeline_data_2, "try-error")) {
      
      # Preparing the content with a link (url from app)
      timeline_data_2$content <- try(html_link_with_app_name(timeline_data_2))
      
      if (!is.null(timeline_data_2$content)) {
        timeline_data_2 <- timeline_data_2[timeline_data_2$start < timeline_data_2$end ,c("_id", "content", "icourse", "start", "end", "app")]
        # Setting the good names to fit timevis
        names(timeline_data_2) <- c("id", "content", "group", "start", "end", "app")
        
        # Preparing the groups for timevis
        attr(timeline_data_2, "groups") <- data.frame(
          id = unique(timeline_data_2$group),
          content = unique(timeline_data_2$group)
        )
      } else { timeline_data_2 <- NULL }
    } else { timeline_data_2 <- NULL }
    
    # Display the timeline when the output is available and when the data is available
    output$apps_timeline_2 <- renderTimevis({
      if (!is.null(timeline_data_2)) {
        timevis( timeline_data_2, groups = attr(timeline_data_2, "groups") )
      }
    })

# Update the Timelines ----------------------------------------------------

    # Update - Changing the timevis window
    observeEvent(selected_app(), {
      if (selected_app() != "All") {
        # Get the dates of start and end of selected app
        start_1 <- as.POSIXct(timeline_data_1()[timeline_data_1()$app == selected_app(),"start"])
        start_2 <- as.POSIXct(timeline_data_2[timeline_data_2$app == selected_app(),"start"])
        end_1 <- as.POSIXct(timeline_data_1()[timeline_data_1()$app == selected_app(),"end"])
        end_2 <- as.POSIXct(timeline_data_2[timeline_data_2$app == selected_app(),"end"])
        # Change dates to have a range (7 days before the start <-> 7 days after the end)
        lubridate::day(start_1) <- lubridate::day(start_1) - 7
        lubridate::day(start_2) <- lubridate::day(start_2) - 7
        lubridate::day(end_1) <- lubridate::day(end_1) + 7
        lubridate::day(end_2) <- lubridate::day(end_2) + 7
        # Center on the selected app (only one of centerItem or setWindow is useful)
        # centerItem("apps_timeline_1", timeline_data_1()[timeline_data_1()$app == selected_app(),"id"])
        # centerItem("apps_timeline_2", timeline_data_2[timeline_data_2$app == selected_app(),"id"])
        # Set the window on the range of dates
        setWindow("apps_timeline_1", start = start_1, end = end_1)
        setWindow("apps_timeline_2", start = start_2, end = end_2)
      }
    })

# Courses Students Plot ---------------------------------------------------

    # Display // UI for the plot of students in courses
    output$courses_students_plot <- renderUI({
      if (!inherits(sdd_users, "try-error")) {
        tagList(
          box( title = "Amount of Students per Course", solidHeader = TRUE,
            width = 10, icon = shiny::icon("user-check", verify_fa = FALSE), collapsible = TRUE,
            collapsed = TRUE, status = "maroon",
            checkboxInput(ns("show_na"), "Show NA's"),
            plotOutput(ns("courses_students"))
          )
        )
      }
    })
    
    # Display // Plot of students in courses
    output$courses_students <- renderPlot({
      
      # Getting the data of students and their course + enrolled
      users <- try(sdd_users$find('{}', fields = '{ "user_login" : true , "icourse" : true , "enrolled" : true}')[c("user_login", "icourse", "enrolled")], silent = TRUE)
      
      # Put off the NA's if show_na is false 
      if (input$show_na == FALSE && !inherits(users, "try-error")) {
        users <- na.omit(users)
      }
      
      # If there are no errors, create the plot of nb of students in courses filled by enrolled or not
      if (!inherits(users, "try-error") && !is.null(users)) {
        ggplot(data = users) +
          geom_bar(mapping = aes(icourse, fill = enrolled)) +
          xlab("Courses") +
          ylab("Number of Students") +
          coord_flip()
      }
    })
  })
}
    
## To be copied in the UI
# mod_home_page_ui("home_page_1")
    
## To be copied in the server
# mod_home_page_server("home_page_1")
