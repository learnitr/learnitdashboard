#' HTML Link with App Names
#' 
#' Description
#'
#' @param data_frame data.frame from which it takes the url and app.
#'
#' @return A vector of html links.
#' @export
#'
#' @examples
#' # Need special data frame
html_link_with_app_name <- function(data_frame) {
  if (!any(!c("alt_url", "url", "app") %in% names(data_frame))) {
    return(glue::glue(r"--[<a href="<<data_frame$url>>"><<data_frame$app>></a> (<a href="<<data_frame$alt_url>>">+</a>)]--", .open = "<<", .close = ">>"))
  }
}