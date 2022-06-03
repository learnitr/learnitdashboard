#' Prepare Style
#' 
#' Prepare a given dataframe's css style, such as background-color, to be rendered in a timeline.
#'
#' @param data_frame data.frame on which prepare the content.
#'
#' @return A vector of css styles
#' @export
#'
#' @examples
#' # Need special data frame
prepare_style <- function(data_frame) {
  if ("type" %in% names(data_frame)) {
    styles <- sapply(data_frame$type, switch,
      "group challenge" = "background-color : #D2B48C; font-weight : bold;",
      "group github" = "background-color : #D2B48C; font-weight : bold;",
      "ind. challenge" = "background-color : #F7CAC9; font-weight : bold;",
      "ind. github" = "background-color : #F7CAC9; font-weight : bold;",
      "h5p" = "background-color : #7FCDCD; font-weight : bold;",
      "learnr" = "background-color : #98FB98; font-weight : bold;",
      "shiny" = "background-color : #92A8D1; font-weight : bold;"
    )
    return(styles)
  }
}