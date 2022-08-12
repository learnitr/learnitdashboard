#' Prepare Url and Alt_Url
#' 
#' Prepare the url and alt_url of a given dataframe, making them an html link.
#'
#' @param data_table The dataframe from which the url and alt_url have to be prepared.
#'
#' @return The given dataframe with the url and alt_url prepared in html link format.
#' @export
#'
#' @examples
#' # Need a special dataframe
prepare_url_alr_url <- function(data_table) {
  # If url and alt_url and template
  if (!any(!c("url", "alt_url","template") %in% names(data_table))) {
    
    # Preparing the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    # Preparing the alt_url in html link
    data_table$alt_url <- paste0('<a href="',data_table$alt_url,'" target="_blank">',data_table$alt_url,'</a>')
    # Preparing the template in html link
    data_table$template <- paste0('<a href="',data_table$template,'" target="_blank">',data_table$template,'</a>')
    
    return(data_table)
  # If url and alt_url
  } else if (!any(!c("url", "alt_url") %in% names(data_table))) {
    
    # Preparing the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    # Preparing the alt_url in html link
    data_table$alt_url <- paste0('<a href="',data_table$alt_url,'" target="_blank">',data_table$alt_url,'</a>')
    
    return(data_table)
  # If only url and avatar
  } else if (!any(!c("url", "avatar") %in% names(data_table))) {
  
    # Preparing the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    # Preparing the alt_url in html link
    data_table$avatar <- paste0('<a href="',data_table$avatar,'" target="_blank">',data_table$avatar,'</a>')
    
    return(data_table)
  # If only url
  } else if ("url" %in% names(data_table)) {
    
    # Preparing the url in html link
    data_table$url <- paste0('<a href="',data_table$url,'" target="_blank">',data_table$url,'</a>')
    
    return(data_table)
  # If none
  } else {
    return(data_table)
  }
}