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
      valueBoxOutput(ns("valuebox_1")),
      valueBoxOutput(ns("valuebox_2")),
      valueBoxOutput(ns("valuebox_3"))
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

# Global Vars -------------------------------------------------------------

    # URL to access databases
    sdd_url <- "mongodb://127.0.0.1:27017/sdd"
    # To connect to them
    sdd_h5p <- try(mongolite::mongo("h5p", url = sdd_url), silent = TRUE)
    sdd_learnr <- try(mongolite::mongo("learnr", url = sdd_url), silent = TRUE)
    sdd_shiny <- try(mongolite::mongo("shiny", url = sdd_url), silent = TRUE)
    sdd_users <- try(mongolite::mongo("users", url = sdd_url), silent = TRUE)
    
# Display Boxes -----------------------------------------------------------

    # Display // Info Box 1
    output$infobox_1 <- renderInfoBox({
      en_nb_std <- try(length(sdd_users$distinct("user_login", query = '{ "enrolled" : "yes" }')), silent = TRUE)
      
      if (!inherits(en_nb_std, "try-error")) {
        infoBox(
          title = "Enrolled Students",
          value = en_nb_std,
          icon = icon("graduation-cap", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("graduation-cap", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 2
    output$infobox_2 <- renderInfoBox({
      all_nb_std <- try(length(sdd_users$distinct("user_login")), silent = TRUE)
      
      if (!inherits(all_nb_std, "try-error")) {
        infoBox(
          title = "All Students",
          value = all_nb_std,
          icon = icon("graduation-cap", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("graduation-cap", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Info Box 3
    output$infobox_3 <- renderInfoBox({
      nb_courses <- try(length(sdd_users$distinct("icourse")), silent = TRUE)
      
      if (!inherits(nb_courses, "try-error")) {
        infoBox(
          title = "Courses",
          value = nb_courses,
          icon = icon("school", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        infoBox(
          title = "",
          icon = icon("school", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Value Box 1
    output$valuebox_1 <- renderValueBox({
      h5p_apps <- try(sdd_h5p$distinct("app"), silent = TRUE)
      h5p_apps <- try(length(h5p_apps[h5p_apps != ""]), silent = TRUE)
      
      if (!inherits(h5p_apps, "try-error")) {
        valueBox(
          value = h5p_apps,
          subtitle = "H5P Apps",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        valueBox(
          subtitle = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Value Box 2
    output$valuebox_2 <- renderValueBox({
      learnr_apps <- try(sdd_learnr$distinct("app"), silent = TRUE)
      learnr_apps <- try(length(learnr_apps[learnr_apps != ""]), silent = TRUE)
      
      if (!inherits(learnr_apps, "try-error")) {
        valueBox(
          value = learnr_apps,
          subtitle = "Learnr Apps",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        valueBox(
          subtitle = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
    
    # Display // Value Box 3
    output$valuebox_3 <- renderValueBox({
      shiny_apps <- try(sdd_shiny$distinct("app"), silent = TRUE)
      shiny_apps <- try(length(shiny_apps[shiny_apps != ""]), silent = TRUE)
      
      if (!inherits(shiny_apps, "try-error")) {
        valueBox(
          value = shiny_apps,
          subtitle = "Shiny Apps",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      } else {
        valueBox(
          subtitle = "",
          icon = icon("gears", verify_fa = FALSE),
          color = "purple"
        )
      }
    })
 
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
      if (input$show_na == FALSE) {
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
