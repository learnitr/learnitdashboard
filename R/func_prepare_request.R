#' Pepare Request
#' 
#' Prepare the request to get data from MongoDB from chosen arguments and a vector containing different ones.
#'
#' @param request_vector Vector of prepared arguments from selected choices.
#' @param args Vector of selected arguments to use in the request.
#'
#' @return A string request with the chosen arguments.
#' @export
#'
#' @examples
#' rv <- c("color" = "red", "type" = "flower")
#' prepare_request(rv, "color")
prepare_request <- function(request_vector, args, type = NULL) {
  if (!is.null(type)) {
    request_vector <- c(request_vector, glue::glue(r"--["type" : "<<type>>"]--", .open = "<<", .close = ">>"))
  }
  request <- paste(request_vector[args[args %in% names(request_vector)]], collapse = " , ")
  return(glue::glue(r"--[{<<request>>}]--", .open = "<<", .close = ">>"))
}