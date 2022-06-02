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
  if (!any(!c("alt_url", "url", "app", "type") %in% names(data_frame))) {
    print(list.files())
    image <- sapply(data_frame$type, switch,
      "group challenge" = "www/images/assign2.png",
      "group github" = "www/images/assign2.png",
      "ind. challenge" = "www/images/assign.png",
      "ind. github" = "www/images/assign.png",
      "h5p" = "www/images/h5p.png",
      "learnr" = "www/images/auto.png",
      "shiny" = "www/images/app.png"
    )
    names(image) <- NULL
    return(glue::glue(r"--[<img src="<<image>>" width="20" height="20"><a href="<<data_frame$url>>"><<data_frame$app>></a> (<a href="<<data_frame$alt_url>>">+</a>)]--", .open = "<<", .close = ">>"))
  }
}