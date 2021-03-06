#' Function to get points, lines, or polygons from OpenStreetMap using 
#' \code{ra.osmsurround.org}. 
#' 
#' @param id A vector of OpenStreetMap IDs. 
#' 
#' @param type Type of data to return. Can be one of \code{"points"}, 
#' \code{"lines"}, \code{"polygons"}, or \code{"data"}. If \code{"data"},
#' a data frame will be returned rather than spatial data. 
#' 
#' @param progress Type of progress bar to display. 
#' 
#' @return Spatial data or data frame. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Get a walking route in York as spatial lines
#' sp_walking_route <- get_osm_surround(1069837, type = "lines")
#' 
#' # Get some polygons for New York
#' sp_new_york <- get_osm_surround(c(369519, 369518, 3954665), "polygons")
#' 
#' # Plot
#' leaflet_plot(sp_new_york)
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @export
get_osm_surround <- function(id, type = "data", progress = "none") {
  
  if (type == "data") {
    
    # Get all ids as data frames
    df <- plyr::ldply(
      id, 
      get_osm_surround_worker, 
      type = type, 
      .progress = progress
    )
    
    return(df)
    
  } else {
    
    # Get all ids as spatial objects
    sp <- plyr::llply(
      id, 
      get_osm_surround_worker, 
      type = type,
      .progress = progress
    )
    
    # Single object bitte
    sp <- sp_bind(sp)
    
    return(sp)
    
  }
  
}


# The worker
get_osm_surround_worker <- function(id, type = "data") {
  
  # Catch
  id <- as.numeric(id)
  type <- stringr::str_to_lower(type)
  
  # message(id)
  
  # Build url string
  url_base <- "http://ra.osmsurround.org/exportRelation/json?relationId="
  url <- stringr::str_c(url_base, id)
  
  # Read text
  text <- tryCatch({
    
    threadr::read_lines(url)
    
  }, error = function(e) {
    
    NULL
    
  })
  
  # Return here
  if (is.null(text)) return(text)
  
  # Parse
  json <- jsonlite::fromJSON(text)
  
  # Extract coordinates
  df <- json$features$geometry$coordinates[[1]][[1]]
  
  # If no features return null
  if (is.null(df)) {
    
    warning("API returned data, but it contains no features.", call. = FALSE)
    return(NULL)
    
  }
  
  # Drop
  df$altitude <- NULL
  
  # Add open street map id
  df$id <- id
  
  # Arrange
  df <- df[, c("id", "latitude", "longitude")]
  
  # 
  if (type == "data") {
    
    # Data frame
    return(df)
     
  } else {
    
    # Promote to sp
    sp <- sp_from_data_frame(df, type = type)
    
    # Overwrite data slot
    sp@data <- data.frame(
      id_osm = id
    )
    
    # Spatial data
    return(sp)
    
  }
  
}
