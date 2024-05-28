#' @details
#' This is a dashboard app to explore 'learnitr' data.
#' 
#' @section Shiny application:
#' 
#' - To launch the Shiny application : use [run_app()]
#' 
#' @keywords internal
"_PACKAGE"

#' @importFrom bslib bs_theme font_google
#' @importFrom dplyr select mutate filter full_join arrange
#' @importFrom DT DTOutput renderDT
#' @importFrom ggplot2 ggplot geom_count aes geom_bar theme_bw coord_flip xlab ylab qplot aes_string labs geom_hline geom_col position_nudge scale_y_discrete
#' @importFrom glue glue
#' @importFrom lubridate year month day floor_date ceiling_date hour minute
#' @importFrom mongolite mongo
#' @importFrom plotly plotlyOutput renderPlotly ggplotly plot_ly
#' @import shiny
#' @import shinydashboardPlus
#' @importFrom shinydashboard dashboardBody infoBox infoBoxOutput menuItem renderInfoBox renderMenu sidebarMenu sidebarMenuOutput tabBox tabItem tabItems
#' @importFrom shinyjs useShinyjs disabled disable enable
#' @importFrom shinyTime timeInput
#' @importFrom stats na.omit rnorm
#' @importFrom thematic thematic_shiny
#' @importFrom tidyr pivot_longer
#' @importFrom timevis timevisOutput renderTimevis timevis centerItem setWindow
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
