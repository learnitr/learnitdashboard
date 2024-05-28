#' is_request function
#'
#' @param input The input to check if there is a request
#'
#' @return TRUE if there is a request about this input, FALSE if not
#' @export
#'
#' @examples
#' # Need a reactive context
is_request <- function(input) {
  !is.null(input) && input != "All" # && input != "NULL"
}
