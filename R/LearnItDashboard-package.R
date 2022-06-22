#' learnitdashboard
#' 
#' This is a dashboard app to explore learnitdown data
#' 
#' @section Application Shiny:
#' 
#' - To launch the Shiny application : use [run_app()]
#' 
#' @docType package
#' @name learnitdashboard-package
#' 
#' 
#' @importFrom bslib bs_theme font_google
#' @import dashboardthemes
#' @importFrom DT DTOutput renderDT
#' @importFrom ggplot2 ggplot geom_count aes geom_bar theme_bw coord_flip xlab ylab qplot
#' @importFrom glue glue
#' @importFrom lubridate year month day floor_date ceiling_date hour minute
#' @import mongolite
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly
#' @import rlang
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @importFrom shinyjs useShinyjs disabled disable enable
#' @import shinyTime
#' @import stats
#' @importFrom thematic thematic_shiny
#' @importFrom tidyr pivot_longer
#' @importFrom timevis timevisOutput renderTimevis timevis centerItem setWindow
#' @importFrom utils head
#' 
NULL
