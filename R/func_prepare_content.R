#' Prepare Content
#' 
#' Prepare a given dataframe's content with html tags, such as links or images, to be rendered in a timeline.
#'
#' @param data_frame data.frame on which prepare the content.
#'
#' @return A vector of html links.
#' @export
#'
#' @examples
#' # Need special data frame
prepare_content <- function(data_frame) {
  if (!any(!c("alt_url", "url", "app", "type") %in% names(data_frame))) {
    image <- sapply(data_frame$type, switch,
      "group challenge" = "https://wp.sciviews.org/sdd-umons-2021/images/list-challenge.png",
      "group github" = "https://wp.sciviews.org/sdd-umons-2021/images/list-assign2.png",
      "ind. challenge" = "https://wp.sciviews.org/sdd-umons-2021/images/list-challenge.png",
      "ind. github" = "https://wp.sciviews.org/sdd-umons-2021/images/list-assign.png",
      "h5p" = "https://wp.sciviews.org/sdd-umons-2021/images/list-h5p.png",
      "learnr" = "https://wp.sciviews.org/sdd-umons-2021/images/list-tuto.png",
      "shiny" = "https://wp.sciviews.org/sdd-umons-2021/images/list-app.png"
    )
    names(image) <- NULL
    # <img src="<<image>>" width="20" height="20">
    return(glue::glue(r"--[<img src="<<image>>"> <a href="<<data_frame$url>>"><<data_frame$app>></a> (<a href="<<data_frame$alt_url>>">+</a>)]--", .open = "<<", .close = ">>"))
  } else if (!any(!c("alt_url", "url", "label") %in% names(data_frame))) {
    return(glue::glue(r"--[<a href="<<data_frame$url>>"><<data_frame$label>> </a> (<a href="<<data_frame$alt_url>>">+</a>)]--", .open = "<<", .close = ">>"))
  }
}