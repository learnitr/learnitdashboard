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
    
    # Template slots for graph
    uiOutput(ns("slot1")),
    uiOutput(ns("slot2")),
    
    # UIoutput to generate a timeline of the different apps
    # uiOutput(ns("ui_apps_timeline_1")),
    
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
    
# Display Boxes -----------------------------------------------------------

    # Display // Info Box 1
    output$infobox_1 <- renderInfoBox({
      
      # The boxes need to have something inside, even empty, so it tests if we can put our data inside
      if (!inherits(h5p_news(), "try-error")) {
        infoBox(
          title = "H5P",
          subtitle = if (h5p_news() > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (h5p_news() > 0) {
            h5p_news()
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 2
    output$infobox_2 <- renderInfoBox({
      
      if (!inherits(learnr_news(), "try-error")) {
        infoBox(
          title = "Learnr",
          subtitle = if (shiny_news() > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (learnr_news() > 0) {
            learnr_news()
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 3
    output$infobox_3 <- renderInfoBox({
      
      if (!inherits(shiny_news(), "try-error")) {
        infoBox(
          title = "Shiny",
          subtitle = if (learnr_news() > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (shiny_news() > 0) {
            shiny_news()
          } else {
            "No changes"
          },
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 4
    output$infobox_4 <- renderInfoBox({
      
      # The boxes need to have something inside, even empty, so it tests if we can put our data inside
      if (!inherits(h5p_news(), "try-error")) {
        infoBox(
          title = "H5P",
          # Show the apps if there are
          subtitle = if (length(attr(h5p_news(), "apps")) > 0) {
            selectInput(ns("h5p_apps_show"), NULL, choices = attr(h5p_news(), "apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (h5p_news() > 0) {
            paste0(length(attr(h5p_news(), "apps")), " apps changed in ", length(attr(h5p_news(), "courses")), " courses")
          } else { "No changes" },
          icon = icon("pencil", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("pencil", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 5
    output$infobox_5 <- renderInfoBox({
      
      if (!inherits(learnr_news(), "try-error")) {
        infoBox(
          title = "Learnr",
          # Show the apps if there are
          subtitle = if (length(attr(learnr_news(), "apps")) > 0) {
            selectInput(ns("learnr_apps_show"), NULL, choices = attr(learnr_news(), "apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (learnr_news() > 0) {
            paste0(length(attr(learnr_news(), "apps")), " apps changed in ", length(attr(learnr_news(), "courses")), " courses")
          } else { "No changes" },
          icon = icon("chalkboard", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("chalkboard", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 6
    output$infobox_6 <- renderInfoBox({
      
      if (!inherits(shiny_news(), "try-error")) {
        infoBox(
          title = "Shiny",
          # Show the apps if there are
          subtitle = if (length(attr(shiny_news(), "apps")) > 0) {
            selectInput(ns("shiny_apps_show"), NULL, choices = attr(shiny_news(), "apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (shiny_news() > 0) {
            paste0(length(attr(shiny_news(), "apps")), " apps changed in ", length(attr(shiny_news(), "courses")), " courses")
          } else { "No changes" },
          icon = icon("tablet", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("tablet", verify_fa = FALSE),
          color = "purple"
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
    #       color = "purple"
    #     )
    #   } else {
    #     valueBox(
    #       value = NULL,
    #       subtitle = "",
    #       icon = icon("gears", verify_fa = FALSE),
    #       color = "purple"
    #     )
    #   }
    # })

# Template slots ----------------------------------------------------------

    output$slot1 <- renderUI({
      if (!inherits(sdd_users, "try-error")) {
        tagList(
          box( title = "Slot 1", solidHeader = TRUE,
               width = 6, icon = shiny::icon("user-check", verify_fa = FALSE), collapsible = TRUE,
               collapsed = TRUE, status = "purple",
               "Content"
          )
        )
      }
    })
    
    output$slot2 <- renderUI({
      if (!inherits(sdd_users, "try-error")) {
        tagList(
          box( title = "Slot 2", solidHeader = TRUE,
               width = 6, icon = shiny::icon("user-check", verify_fa = FALSE), collapsible = TRUE,
               collapsed = TRUE, status = "purple",
               "Content"
          )
        )
      }
    })
    
# Courses Students Plot ---------------------------------------------------

    # Display // UI for the plot of students in courses
    output$courses_students_plot <- renderUI({
      if (!inherits(sdd_users, "try-error")) {
        tagList(
          box( title = "Amount of Students per Course", solidHeader = TRUE,
            width = 10, icon = shiny::icon("user-check", verify_fa = FALSE), collapsible = TRUE,
            collapsed = TRUE, status = "purple",
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
