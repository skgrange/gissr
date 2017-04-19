#' Function to get OpenStreetMap boundaries as spatial polygons. 
#' 
#' \code{get_osm_boundary} accesses \href{http://polygons.openstreetmap.fr/index.py}{this}
#' polygon creator tool. 
#' 
#' @param id A vector of OpenStreetMap relations. An integer key.
#' @param print_query Should the query to the API be printed? 
#' @param progress Type of progress bar to display. Default is \code{"none"}
#' 
#' @return SpatialPolygonsDataFrame with WGS84 projection. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Get North York Moors National Park boundary
#' sp_moors <- get_osm_boundary(409150)
#' 
#' }
#' 
#' @export
get_osm_boundary <- function(id, print_query = FALSE, progress = "none") {
  
  # Parse
  id <- stringr::str_trim(id)
  
  # Vectorise function
  sp_list <- plyr::llply(id, osm_boundary_worker, print_query = print_query, 
                         .progress = progress)
  
  # Bind list
  sp <- sp_bind(sp_list)
  
  # Drop unique observation id
  sp@data$id <- NULL
  
  # Return
  sp
  
}


# No export
osm_boundary_worker <- function(id, print_query) {
  
  # Build query
  url <- stringr::str_c("http://polygons.openstreetmap.fr/get_wkt.py?id=", id, 
                        "&params=0")
  
  # Message
  if (print_query) message(url)
  
  # Get wkt
  text <- tryCatch({
    
    # Read text
    suppressWarnings(
      readLines(url, warn = FALSE)
    )
    
  }, error = function(e) {
    
    # If error return null
    message(stringr::str_c("'id' ", id, " not found..."))
    NULL
    
  })
  
  # If null then return now
  if (is.null(text)) return(NULL)
  
  # Drop preamble/projection infomation
  text <- stringr::str_replace(text, "SRID=4326;", "")
  
  # Promote to spatial
  sp <- sp_from_wkt(text, verbose = FALSE)
  
  # Add projection
  sp <- sp_transform(sp, to = projection_wgs84(), warn = FALSE)
  
  # Give a data slot
  sp <- sp_promote(sp)
  
  # Add open street map id to data slot
  sp@data$osm_id <- id
  
  return(sp)
  
}
