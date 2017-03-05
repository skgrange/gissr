#' Function to scrape OpenStreetMap's node XML documents for data. 
#' 
#' @param id OpenStreetMap node ID. 
#' 
#' @author Stuart K. Grange
#' 
#' @import dplyr
#' 
#' @return Named list. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get data for a peak in the Yorkshire Dales
#' get_osm_node_data(312154686)
#' 
#' # Get same data but for another peak in the North York Moors
#' get_osm_node_data(c(312154686, 1018516271))
#' 
#' }
#' 
#' @export
get_osm_node_data <- function(id) {
  
  # Parse id
  id <- stringr::str_replace(id, "^n", "")
  
  # Build urls
  url <- stringr::str_c("http://www.openstreetmap.org/api/0.6/node/", id)
  
  # Get all data 
  list_id <- plyr::llply(url, get_osm_node_data_worker)
  
  # Collapse
  list_return <- list(
    values = bind_rows(lapply(list_id, `[[`, "values")),
    attributes = bind_rows(lapply(list_id, `[[`, "attributes"))
  )
  
  # Data types
  list_return$attributes <- list_return$attributes %>% 
    mutate(id = as.numeric(id), # sometimes too large for integer
           latitude = as.numeric(lat), 
           longitude = as.numeric(lon),
           version = as.integer(version),
           changeset = as.integer(changeset),
           uid = as.integer(uid),
           visible = as.logical(visible)) %>% 
    select(-lat,
           -lon) %>% 
    data.frame()
  
  # Return
  list_return
  
}


get_osm_node_data_worker <- function(url) {
  
  # Get document
  text <- readLines(url, warn = FALSE)
    
  # Parse xml
  list_xml <- XML::xmlToList(text)
  
  # The content
  list_xml_content <- list_xml$node
  
  # Get attributes
  if (!is.list(list_xml_content)) {
    
    df_attributes <- as.data.frame(t(list_xml_content), 
                                   stringsAsFactors = FALSE)
    
    
    df <- data.frame()
    
  } else {
    
    df_attributes <- as.data.frame(t(list_xml_content$.attrs), 
                                   stringsAsFactors = FALSE)
    
    # Extract data from list
    list_tidy <- lapply(list_xml_content, extract_osm_tags)
    
    # Make tidy data frame
    df <- bind_rows(list_tidy) %>% 
      mutate(id = as.numeric(df_attributes$id)) %>% 
      threadr::arrange_left("id")
    
  }
  
  # Create list
  if (nrow(df) != 0) {
    
    list_return <- list(
      values = df,
      attributes = df_attributes
    )
    
  } else {
    
    list_return <- list(
      attributes = df_attributes
    )
    
  }
  
  
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
    
  } else if (names[1] == "ref") {
    
    df <- data.frame(reference = as.numeric(unname(x)), stringsAsFactors = FALSE)
    
  } else if (names[1] == "type" & names[2] == "ref" & names[3] == "role") {
    
    df <- data.frame(
      reference = as.numeric(unname(x[2])),
      type = unname(x[1]),
      role = unname(x[3]),
      stringsAsFactors = FALSE)
    
  } else {
    
    # Return empty data frame
    df <- data.frame()
    
  }
  
  # Return
  df
  
}



#' Function to scrape OpenStreetMap's way XML documents for data. 
#' 
#' @param id A vector os OpenStreetMap way IDs. 
#' 
#' @author Stuart K. Grange
#' 
#' @import dplyr
#' 
#' @return Named list or a list of named lists. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get data for York Minster
#' get_osm_way_data(28750371)
#' 
#' # Get data for two ways within a walking route (a relation)
#' get_osm_way_data(c(52461049, 233498222))
#' 
#' }
#' 
#' @export
get_osm_way_data <- function(id, progress = "none") {
  
  # Parse id
  id <- stringr::str_replace(id, "^w", "")
  
  if (length(id) == 1) {
    
    list_way <- get_osm_way_data_worker(id)
    
  } else {
    
    list_way <- plyr::llply(id, get_osm_way_data_worker, .progress = progress)
    
  }
  
  # Return
  list_way
  
}


get_osm_way_data_worker <- function(id) {
  
  # Build url
  url <- stringr::str_c("http://www.openstreetmap.org/api/0.6/way/", id)
  
  # Read
  text <- readLines(url, warn = FALSE)
  
  # Parse xml
  list_xml <- XML::xmlToList(text)
  
  # The content
  list_xml_content <- list_xml$way
  
  # Get attributes
  df_attributes <- as.data.frame(t(list_xml_content$.attrs), 
                                 stringsAsFactors = FALSE)
  
  # Extract data from list
  list_tidy <- lapply(list_xml_content, extract_osm_tags)
  
  # Split observational units
  list_tidy_relations <- list_tidy[names(list_tidy) == "nd"]
  list_tidy <- list_tidy[!names(list_tidy) == "nd"]
  
  # Get relations
  relations <- suppressWarnings(bind_rows(list_tidy_relations)$reference)
  
  # Make tidy data frame
  df <- bind_rows(list_tidy) %>% 
    mutate(id = as.numeric(df_attributes$id)) %>% 
    threadr::arrange_left("id")
  
  # Create list
  list_return <- list(
    values = df,
    attributes = df_attributes,
    relations = relations
  )
  
  # Return
  list_return
  
}



#' Function to scrape OpenStreetMap's relation XML documents for data. 
#' 
#' @param id OpenStreetMap relation ID. 
#' 
#' @author Stuart K. Grange
#' 
#' @import dplyr
#' 
#' @return Named list. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get data for Forest of Bowland
#' get_osm_relation_data(6399292)
#' 
#' }
#' 
#' @export
get_osm_relation_data <- function(id) {
  
  # Parse id
  id <- stringr::str_replace(id, "^r", "")
  
  # Build url
  url <- stringr::str_c("http://www.openstreetmap.org/api/0.6/relation/", id)
  
  # Read
  text <- readLines(url, warn = FALSE)
  
  # Parse xml
  list_xml <- XML::xmlToList(text)
  
  # The content
  list_xml_content <- list_xml$relation
  
  # Get attributes
  df_attributes <- as.data.frame(t(list_xml_content$.attrs), 
                                 stringsAsFactors = FALSE)
  
  # Extract data from list
  list_tidy <- lapply(list_xml_content, extract_osm_tags)
  
  # Split observational units
  list_tidy_members <- list_tidy[names(list_tidy) == "member"]
  list_tidy <- list_tidy[!names(list_tidy) == "member"]
  
  # Get relations
  df_members <- suppressWarnings(bind_rows(list_tidy_members))
  
  # Make tidy data frame
  df <- bind_rows(list_tidy) %>% 
    mutate(id = as.numeric(df_attributes$id)) %>% 
    threadr::arrange_left("id")
  
  # Create list
  list_return <- list(
    values = df,
    attributes = df_attributes,
    members = df_members
  )
  
  # Return
  list_return
  
}
