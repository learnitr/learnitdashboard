#' Prepare Request
#' 
#' Prepare the request to get data from MongoDB from chosen arguments and a
#' vector containing different ones.
#'
#' @param request_vector Vector of prepared arguments from selected choices.
#' @param args Vector of selected arguments to use in the request.
#' @param type Type of table to query. Can be h5p, learnr or shiny.
#' 
#'
#' @return A string request with the chosen arguments.
#' @export
#'
#' @examples
#' rv <- c("color" = "red", "type" = "flower")
#' prepare_request(rv, "color")
prepare_request <- function(request_vector, args, type = NULL) {
  # If type is given, prepare the type request part and add it to request_vector
  if (!is.null(type)) {
    request_vector <- c(request_vector, "type" =
        glue::glue(r"--["type" : "<<type>>"]--", .open = "<<", .close = ">>"))
  }
  
  # If request_vector isn't empty, creates the request
  if (request_vector[1] != "empty") {
    request <- paste(request_vector[args[args %in% names(request_vector)]],
      collapse = " , ")
    return(glue::glue(r"--[{<<request>>}]--", .open = "<<", .close = ">>"))
  # If request_vector is empty, create an empty request with or without the type
  } else {
    if ("type" %in% names(request_vector)) {
      return(glue::glue(r"--[{<<request_vector["type"]>>}]--",
        .open = "<<", .close = ">>"))
    } else {
      return("{}")
    }
  }
}
