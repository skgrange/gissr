#' Function to scrape OpenStreetMap's node XML document for data. 
#' 
#' @return Named list with \code{values} and \code{attributes} data frames. 
#' 
#' @param id OpenStreetMap relation ID. 
#' 
#' @author Stuart K. Grange
#' 
#' @import dplyr
#' 
#' @export
get_osm_node_data <- function(id) {
  
  # Url
  url <- stringr::str_c("http://www.openstreetmap.org/api/0.6/node/", id)
  # way
  
  # Get document
  text <- readLines(url, warn = FALSE)
  
  # Parse xml
  list_xml <- XML::xmlToList(text)
  
  # The content
  list_xml_content <- list_xml$node
  # list_xml_content <- list_xml$way
  
  # Get attributes
  df_attributes <- as.data.frame(t(list_xml_content$.attrs))
  
  # Drop attributes
  # list_xml_content$.attrs <- NULL
  
  # Extract data from list
  list_tidy <- lapply(list_xml_content, extract_osm_tags)
  
  # Make tidy data frame
  df <- bind_rows(list_tidy) %>% 
    mutate(id = as.integer(df_attributes$id)) %>% 
    threadr::arrange_left("id")
  
  # Create list
  list_return <- list(
    values = df,
    attributes = df_attributes
  )
  
  # Return
  list_return
  
}


# No export needed
extract_osm_tags <- function(x) {
  
  # Get name vector
  names <- names(x)
  
  # Get data if needed
  if (names[1] == "k" & names[2] == "v") {
    
    df <- data.frame(
      variable = unname(x[1]),
      value = unname(x[2]),
      stringsAsFactors = FALSE
    )
    
  } else {
    
    # Return empty data frame
    df <- data.frame()
    
  }
  
  # Return
  df
  
}
