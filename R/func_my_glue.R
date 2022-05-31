#' My Glue
#'
#' @param ... Expression to be glued with "<<" and ">>" delimiters.
#'
#' @return The result of the glue::glue function using "<<" and ">>" delemiters.
#' @export
#'
#' @examples
#' my_glue("2 + 2 = <<2+2>>")
my_glue <- function (...) {
  return(glue::glue(..., .open = "<<", .close = ">>"))
}