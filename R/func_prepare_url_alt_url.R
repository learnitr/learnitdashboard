#' Prepare Url and Alt_Url
#' 
#' Prepare the url and alt_url of a given dataframe, making them an html link.
#'
#' @param data_table The dateframe from wich the url and alt_url have to be prepared.
#'
#' @return The given dataframe with the url and alt_url prepared in html link format.
#' @export
#'
#' @examples
#' # Need a special dataframe
prepare_url_alr_url <- function(data_table) {
  # If url and alt_url
  if (!any(!c("url", "alt_url") %in% names(data_table))) {
    
    # Preparinf the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    # Preparinf the alt_url in html link
    data_table$alt_url <- paste0('<a href="',data_table$alt_url,'" target="_blank">',data_table$alt_url,'</a>')
    
    return(data_table)
  # If only url
  } else if ("url" %in% names(data_table)) {
    
    # Preparinf the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    
    return(data_table)
  # If none
  } else {
    return(data_table)
  }
}