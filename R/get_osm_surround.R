#' Function to get points, lines, or polygons from OpenStreetMap using 
#' \code{ra.osmsurround.org}. 
#' 
#' @param id A vector of OpenStreetMap IDs. 
#' 
#' @param type Type of data to return. Can be one of \code{"points"}, 
#' \code{"lines"}, \code{"polygons"}, or \code{"data"}. If \code{"data"},
#' a data frame will be returned rather than spatial data. 
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
get_osm_surround <- function(id, type = "data") {
  
  if (type == "data") {
    
    # Get all ids as data frames
    df <- plyr::ldply(id, get_osm_surround_worker, type = type)
    
    return(df)
    
  } else {
    
    # Get all ids as spatial objects
    sp <- plyr::llply(id, get_osm_surround_worker, type = type)
    
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
  
  # Build url string
  # http://ra.osmsurround.org/exportRelation/gpx?relationId=
  url_base <- "http://ra.osmsurround.org/exportRelation/json?relationId="
  # url_extra <- "&noCache=true&_noCache=on"
  url <- stringr::str_c(url_base, id)
  
  # Read text
  text <- threadr::read_lines(url)
  
  # Parse
  json <- threadr::read_json(text)
  
  # Extract coordinates
  df <- json$features$geometry$coordinates[[1]][[1]]
  
  # Check
  if (is.null(df)) 
    stop("API returned data, but it contains no features.", call. = FALSE)
  
  # Drop
  df$altitude <- NULL
  
  # Add
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
