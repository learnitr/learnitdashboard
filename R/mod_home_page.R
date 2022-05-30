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
      # valueBoxOutput(ns("valuebox_1")),
      # valueBoxOutput(ns("valuebox_2")),
      # valueBoxOutput(ns("valuebox_3"))
    ),
    
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
    
# Global Vars -------------------------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to users
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    
# Display Boxes -----------------------------------------------------------

    # Display // Info Box 1
    output$infobox_1 <- renderInfoBox({
      
      if (!inherits(h5p_news(), "try-error")) {
        infoBox(
          title = "H5P Entries",
          value = nrow(h5p_news()),
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
          title = "Learnr Entries",
          value = nrow(learnr_news()),
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
          title = "Shiny Entries",
          value = nrow(shiny_news()),
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
    # 
    # # Display // Value Box 2
    # output$valuebox_2 <- renderValueBox({
    #   
    #   if (!inherits(learnr_news(), "try-error")) {
    #     valueBox(
    #       value = paste0(nrow(learnr_news()), " entries"),
    #       subtitle = "Learnr Apps",
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
    # 
    # # Display // Value Box 3
    # output$valuebox_3 <- renderValueBox({
    #   
    #   if (!inherits(shiny_news(), "try-error")) {
    #     valueBox(
    #       value = paste0(nrow(shiny_news()), " entries"),
    #       subtitle = "Shiny Apps",
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
 
    # Display // UI for the plot of students in courses
    output$courses_students_plot <- renderUI({
      if (!inherits(sdd_users, "try-error")) {
        tagList(
          box( title = "Amount of Students per Course", solidHeader = TRUE,
            width = 10, icon = shiny::icon("chart-column", verify_fa = FALSE), collapsible = TRUE,
            background = "purple",
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
