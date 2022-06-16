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
    uiOutput(ns("slot3")),
    
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
    events_news <- reactive({all_vars$right_sidebar_vars$events_news})
    # h5p_news <- reactive({all_vars$right_sidebar_vars$h5p_news})
    # learnr_news <- reactive({all_vars$right_sidebar_vars$learnr_news})
    # shiny_news <- reactive({all_vars$right_sidebar_vars$shiny_news})
    selected_news_time <- reactive({all_vars$right_sidebar_vars$selected_news_time})
    selected_course <- reactive({all_vars$right_sidebar_vars$selected_course})
    selected_module <- reactive({all_vars$right_sidebar_vars$selected_module})
    selected_app <- reactive({all_vars$right_sidebar_vars$selected_app})
    
# Global Vars -------------------------------------------------------------
    
    # R CMD check preparation
    if (!exists("sdd_apps")) {
      sdd_apps <- NULL
    }
    if (!exists("sdd_planning")) {
      sdd_planning <- NULL
    }
    if (!exists("sdd_users2")) {
      sdd_users2 <- NULL
    }
    if (!exists("courses_init")) {
      courses_init <- NULL
    }
    if (!exists("modules_init")) {
      modules_init <- NULL
    }
    if (!exists("apps_init")) {
      apps_init <- NULL
    }
    if (!exists("users2_init")) {
      users2_init <- NULL
    }
    
# Display Boxes -----------------------------------------------------------

    # Display // Info Box 1
    output$infobox_1 <- renderInfoBox({
      
      # The boxes need to have something inside, even empty, so it tests if we can put our data inside
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "H5P",
          subtitle = if (attr(events_news(), "h5p_nb_apps") > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (attr(events_news(), "h5p_nb_apps") > 0) {
            attr(events_news(), "h5p_nb_apps")
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
      
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "Learnr",
          subtitle = if (attr(events_news(), "learnr_nb_apps") > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (attr(events_news(), "learnr_nb_apps") > 0) {
            attr(events_news(), "learnr_nb_apps")
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
      
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "Shiny",
          subtitle = if (attr(events_news(), "shiny_nb_apps") > 0) {
            paste0("New entries since : ", selected_news_time())
          } else {NULL},
          # Show the amount of changes if there are some
          value = if (attr(events_news(), "shiny_nb_apps") > 0) {
            attr(events_news(), "shiny_nb_apps")
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
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "H5P",
          # Show the apps if there are
          subtitle = if (length(attr(events_news(), "h5p_apps")) > 0) {
            selectInput(ns("h5p_apps_show"), NULL, choices = attr(events_news(), "h5p_apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (attr(events_news(), "h5p_nb_apps") > 0) {
            paste0(length(attr(events_news(), "h5p_apps")), " apps changed in ", length(attr(events_news(), "h5p_courses")), " courses")
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
      
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "Learnr",
          # Show the apps if there are
          subtitle = if (length(attr(events_news(), "learnr_apps")) > 0) {
            selectInput(ns("learnr_apps_show"), NULL, choices = attr(events_news(), "learnr_apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (attr(events_news(), "learnr_nb_apps") > 0) {
            paste0(length(attr(events_news(), "learnr_apps")), " apps changed in ", length(attr(events_news(), "learnr_courses")), " courses")
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
      
      if (!inherits(events_news(), "try-error")) {
        infoBox(
          title = "Shiny",
          # Show the apps if there are
          subtitle = if (length(attr(events_news(), "shiny_apps")) > 0) {
            selectInput(ns("shiny_apps_show"), NULL, choices = attr(events_news(), "shiny_apps"))
          } else {NULL},
          # Show the amoun of apps that changed and in how much courses
          value = if (attr(events_news(), "shiny_nb_apps") > 0) {
            paste0(length(attr(events_news(), "shiny_apps")), " apps changed in ", length(attr(events_news(), "shiny_courses")), " courses")
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

    # Slot for global home information
    output$slot1 <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        req(selected_course())
        
        # Getting the data to display
        if (selected_course() != "All") {
          course <- selected_course()
          attr(course, "title") <- courses_init[courses_init$icourse == selected_course(), "ictitle"][1]
          attr(course, "start") <- courses_init[courses_init$icourse == selected_course(), "start"][1]
          attr(course, "end") <- courses_init[courses_init$icourse == selected_course(), "end"][1]
          attr(course, "url") <- courses_init[courses_init$icourse == selected_course(), "url"][1]
          attr(course, "alt_url") <- courses_init[courses_init$icourse == selected_course(), "alt_url"][1]
        }
        
        return(
          box(
            title = if (selected_course() != "All") {
              paste0("Course : ", selected_course())
            } else {
              "Courses"
            },
            solidHeader = TRUE,
            width = 4, icon = shiny::icon("book-open", verify_fa = FALSE),
            collapsible = TRUE, collapsed = FALSE, status = "purple",
            # Box content :
            if (selected_course() != "All") {
              tagList(
                h4("Course Title"),
                attr(course, "title"),
                h4("Course Start"),
                attr(course, "start"),
                h4("Course End"),
                attr(course, "end"),
                h4("Course Url"),
                tags$a(attr(course, "url"), href = attr(course, "url")),
                h4("Course Alt Url"),
                tags$a(attr(course, "alt_url"), href = attr(course, "alt_url"))
              )
            } else {
              tagList(
                h4("ICourses"),
                length(unique(courses_init[,"icourse"])),
                h4("Units"),
                length(unique(courses_init[,"iunit"])),
                h4("Sections"),
                length(unique(courses_init[,"section"])),
                h4("Iclass"),
                length(unique(courses_init[,"iclass"]))
              )
            }
          )
        )
      }
    })
    
    # Slot for global home information
    output$slot2 <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        req(selected_module())
        
        # Getting the data to display
        if (selected_module() != "All") {
          module <- selected_module()
          attr(module, "title") <- modules_init[modules_init$module == selected_module(), "title"][1]
          attr(module, "start") <- modules_init[modules_init$module == selected_module(), "start"][1]
          attr(module, "end") <- modules_init[modules_init$module == selected_module(), "end"][1]
          attr(module, "url") <- modules_init[modules_init$module == selected_module(), "url"][1]
          attr(module, "alt_url") <- modules_init[modules_init$module == selected_module(), "alt_url"][1]
        }
        
        tagList(
          box(
            title = if (selected_module() != "All") {
              paste0("Module : ", selected_module())
            } else {
              "Modules"
            },
            solidHeader = TRUE,
            width = 4, icon = shiny::icon("shapes", verify_fa = FALSE),
            collapsible = TRUE, collapsed = FALSE, status = "purple",
            # Box content :
            if (selected_module() != "All") {
              tagList(
                h4("Module Title"),
                attr(module, "title"),
                h4("Module Start"),
                attr(module, "start"),
                h4("Module End"),
                attr(module, "end"),
                h4("Module Url"),
                tags$a(attr(module, "url"), href = attr(module, "url")),
                h4("Module Alt Url"),
                tags$a(attr(module, "alt_url"), href = attr(module, "alt_url"))
              )
            } else {
              tagList(
                h4("Modules"),
                length(unique(modules_init[,"module"]))
              )
            }
          )
        )
      }
    })
    
    # Slot for global home information
    output$slot3 <- renderUI({
      if (!inherits(sdd_apps, "try-error")) {
        req(selected_app())
        
        # Getting the data to display
        if (selected_app() != "All") {
          app <- selected_app()
          attr(app, "type") <- apps_init[apps_init$app == selected_app(), "type"][1]
          attr(app, "start") <- apps_init[apps_init$app == selected_app(), "start"][1]
          attr(app, "end") <- apps_init[apps_init$app == selected_app(), "end"][1]
          attr(app, "url") <- apps_init[apps_init$app == selected_app(), "url"][1]
          attr(app, "alt_url") <- apps_init[apps_init$app == selected_app(), "alt_url"][1]
        }
        
        return(
          box( 
            title = if (selected_app() != "All") {
              paste0("App : ", selected_app())
            } else {
              "Apps"
            },
            solidHeader = TRUE,
            width = 4, icon = shiny::icon("tablet", verify_fa = FALSE),
            collapsible = TRUE, collapsed = FALSE, status = "purple",
            # Box content :
            if (selected_app() != "All") {
              tagList(
                h4("App Type"),
                attr(app, "type"),
                h4("App Start"),
                attr(app, "start"),
                h4("App End"),
                attr(app, "end"),
                h4("App Url"),
                tags$a(attr(app, "url"), href = attr(app, "url")),
                h4("App Alt Url"),
                tags$a(attr(app, "alt_url"), href = attr(app, "alt_url"))
              )
            } else {
              tagList(
                h4("Apps"),
                length(unique(apps_init[,"app"])),
                h4("Types"),
                length(unique(apps_init[,"type"]))
              )
            }
          )
        )
      }
    })
    
# Courses Students Plot ---------------------------------------------------

    # Display // UI for the plot of students in courses
    output$courses_students_plot <- renderUI({
      if (!inherits(sdd_users2, "try-error")) {
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
      
      users <- NULL
      # Getting the data of students and their course + enrolled
      if (!inherits(sdd_users2, "try-error")) {
        users <- users2_init[,c("login", "icourse", "enrolled")]
      }
      # users <- try(sdd_users2$find('{}', fields = '{ "login" : true , "icourse" : true , "enrolled" : true, "_id" : false}'), silent = TRUE)
      
      # Put off the NA's if show_na is false 
      if (input$show_na == FALSE && length(users) > 0) {
        users <- na.omit(users)
      }
      
      # If there are no errors, create the plot of nb of students in courses filled by enrolled or not
      if (!is.null(users)) {
        icourse <- NULL
        enrolled <- NULL
        users$enrolled <- ifelse(users$enrolled == TRUE, "yes", "no")
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
