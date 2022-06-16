#' Get Users
#' 
#' Get a vector of users_id named by logins and lastnames from a selected course and with enrolled only or everyone
#'
#' @param selected_course Selected course, can be "All" or specific.
#' @param only_enrolled Only the enrolled users or everyone.
#' @param users2_init The table from which it gets the users.
#' @param acad_year The academic year of the users.
#'
#' @return A vector of users_id name by logins and lastnames
#' @export
#'
#' @examples
#' # Need a reactive context and a MongoDB database
get_users <- function(selected_course, only_enrolled, users2_init, acad_year) {
  
  # Case 1 : Course is selected and Enrolled is TRUE
  if (selected_course != "All") {
    if (only_enrolled == TRUE) {
      
      # --- For request to users2 ---
      # Preparing the request
      # users_request <- glue::glue(r"--[
      #                             [{ "$match" : {
      #                             "icourse" : "<<selected_course>>",
      #                             "enrolled" : true,
      #                             "acad_year" : "<<acad_year>>"
      #                             }},
      #                             { "$group": {
      #                             "_id": "$user", 
      #                             "ilastname": { "$first": "$ilastname" }, 
      #                             "login": { "$first": "$login" }
      #                             }}]
      # ]--", .open = "<<", .close = ">>")
      # usersdf <- sdd_users2$aggregate(users_request)
      # --- For request to users2 ---
      
      usersdf <- unique(users2_init[users2_init$icourse == selected_course & users2_init$enrolled == only_enrolled & users2_init$acad_year == acad_year, c("user", "ilastname", "login")])
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$user, names = users_names) # `_id`
      return(users)
      
    # Case 2 : Course is selected and Enrolled is FALSE
    } else {
      
      # --- For request to users2 ---
      # Preparing the request
      # users_request <- glue::glue(r"--[
      #                             [{ "$match" : {
      #                             "icourse" : "<<selected_course>>",
      #                             "acad_year" : "<<acad_year>>"
      #                             }},
      #                             { "$group": {
      #                             "_id": "$user", 
      #                             "ilastname": { "$first": "$ilastname" }, 
      #                             "login": { "$first": "$login" }
      #                             }}]
      # ]--", .open = "<<", .close = ">>")
      # usersdf <- sdd_users2$aggregate(users_request)
      # --- For request to users2 ---
      
      usersdf <- unique(users2_init[users2_init$icourse == selected_course & users2_init$acad_year == acad_year, c("user", "ilastname", "login")])
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$user, names = users_names) # `_id`
      return(users)
    }
    
  # Case 3 : Course is not selected and Enrolled is TRUE
  } else {
    # If only the enrolled
    if (only_enrolled == TRUE) {
      
      # --- For request to users2 ---
      # Preparing the request
      # users_request <- glue::glue(r"--[
      #                             [{ "$match" : {
      #                             "enrolled" : true,
      #                             "acad_year" : "<<acad_year>>"
      #                             }},
      #                             { "$group": {
      #                             "_id": "$user", 
      #                             "ilastname": { "$first": "$ilastname" }, 
      #                             "login": { "$first": "$login" }
      #                             }}]
      # ]--", .open = "<<", .close = ">>")
      # usersdf <- sdd_users2$aggregate(users_request)
      # --- For request to users2 ---
      
      usersdf <- unique(users2_init[users2_init$enrolled == only_enrolled & users2_init$acad_year == acad_year, c("user", "ilastname", "login")])
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$user, names = users_names) # `_id`
      return(users)
      
    # Case 4 : Course is not selected and Enrolled is FALSE
    } else {
      
      # --- For request to users2 ---
      # Preparing the request
      # users_request <- glue::glue(r"--[
      #                             [{ "$match" : {
      #                             "acad_year" : "<<acad_year>>"
      #                             }},
      #                             { "$group": {
      #                             "_id": "$user", 
      #                             "ilastname": { "$first": "$ilastname" }, 
      #                             "login": { "$first": "$login" }
      #                             }}]
      # ]--", .open = "<<", .close = ">>")
      # usersdf <- sdd_users2$aggregate(users_request)
      # --- For request to users2 ---
      
      usersdf <- unique(users2_init[users2_init$acad_year == acad_year, c("user", "ilastname", "login")])
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$user, names = users_names) # `_id`
      return(users)
    }
  }
}