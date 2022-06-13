#' Get Users
#' 
#' Get a vector of users_id named by logins and lastnames from a selected course and with enrolled only or everyone
#'
#' @param selected_course Selected course, can be "All" or specific
#' @param only_enrolled Only the enrolled users or everyone
#' @param sdd_users2 The table from which we get the users
#'
#' @return A vector of users_id name by logins and lastnames
#' @export
#'
#' @examples
#' Need a reactive context and a MongoDB database
get_users <- function(selected_course, only_enrolled, sdd_users2) {
  
  # Case 1 : Course is selected and Enrolled is TRUE
  if (selected_course != "All") {
    if (only_enrolled == TRUE) {
      # Preparing the request
      users_request <- glue::glue(r"--[
                                  [{ "$match" : {
                                  "icourse" : "<<selected_course>>",
                                  "enrolled" : true,
                                  "acad_year" : "2021-2022"
                                  }},
                                  { "$group": {
                                  "_id": "$user", 
                                  "ilastname": { "$first": "$ilastname" }, 
                                  "login": { "$first": "$login" }
                                  }}]
      ]--", .open = "<<", .close = ">>")
      usersdf <- sdd_users2$aggregate(users_request)
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$`_id`, names = users_names)
      return(users)
      
    # Case 2 : Course is selected and Enrolled is FALSE
    } else {
      # Preparing the request
      users_request <- glue::glue(r"--[
                                  [{ "$match" : {
                                  "icourse" : "<<selected_course>>",
                                  "acad_year" : "2021-2022"
                                  }},
                                  { "$group": {
                                  "_id": "$user", 
                                  "ilastname": { "$first": "$ilastname" }, 
                                  "login": { "$first": "$login" }
                                  }}]
      ]--", .open = "<<", .close = ">>")
      usersdf <- sdd_users2$aggregate(users_request)
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$`_id`, names = users_names)
      return(users)
    }
    
  # Case 3 : Course is not selected and Enrolled is TRUE
  } else {
    # If only the enrolled
    if (only_enrolled == TRUE) {
      # Preparing the request
      users_request <- glue::glue(r"--[
                                  [{ "$match" : {
                                  "enrolled" : true,
                                  "acad_year" : "2021-2022"
                                  }},
                                  { "$group": {
                                  "_id": "$user", 
                                  "ilastname": { "$first": "$ilastname" }, 
                                  "login": { "$first": "$login" }
                                  }}]
      ]--", .open = "<<", .close = ">>")
      usersdf <- sdd_users2$aggregate(users_request)
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$`_id`, names = users_names)
      return(users)
      
    # Case 4 : Course is not selected and Enrolled is FALSE
    } else {
      # Preparing the request
      users_request <- glue::glue(r"--[
                                  [{ "$match" : {
                                  "acad_year" : "2021-2022"
                                  }},
                                  { "$group": {
                                  "_id": "$user", 
                                  "ilastname": { "$first": "$ilastname" }, 
                                  "login": { "$first": "$login" }
                                  }}]
      ]--", .open = "<<", .close = ">>")
      usersdf <- sdd_users2$aggregate(users_request)
      # Getting out the NA's and ordering by names and then structure the good names
      usersdf <- na.omit(usersdf)
      usersdf <- usersdf[order(usersdf$ilastname),]
      users_names <- paste0(tools::toTitleCase(tolower(usersdf$ilastname)), " (", usersdf$login, ")")
      users <- structure(usersdf$`_id`, names = users_names)
      return(users)
    }
  }
}