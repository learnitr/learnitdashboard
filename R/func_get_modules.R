#' Get Modules
#' 
#' Get a vector of module named by module and title from a selected course
#'
#' @param selected_course Selected course, can be "All" or specific.
#' @param modules_init The table from which it gets the modules.
#' @param acad_year The academic year of the users.
#'
#' @return A vector of module name by module and title
#' @export
#'
#' @examples
#' # Need a reactive context and a MongoDB database
get_modules <- function(selected_course, modules_init, acad_year) {
  
  # Case 1 : Course is selected
  if (selected_course != "All") {
    modulesdf <- unique(modules_init[
      modules_init$icourse == selected_course &
        modules_init$acad_year == acad_year, c("module", "title")])
    # Getting out the NA's and ordering by names and then structure the good names
    modulesdf <- na.omit(modulesdf)
    modulesdf <- modulesdf[order(modulesdf$module),]
    modules_names <- paste0(modulesdf$module, " (", modulesdf$title, ")")
    modules <- structure(modulesdf$module, names = modules_names) # `_id`
    return(modules)
  
  # Case 2 : Course is not selected
  } else {
    modulesdf <- unique(modules_init[
      modules_init$acad_year == acad_year, c("module", "title")])
    # Getting out the NA's and ordering by names and then structure the good names
    modulesdf <- na.omit(modulesdf)
    modulesdf <- modulesdf[order(modulesdf$module),]
    modules_names <- paste0(modulesdf$module, " (", modulesdf$title, ")")
    modules <- structure(modulesdf$module, names = modules_names) # `_id`
    return(modules)
  }
}
