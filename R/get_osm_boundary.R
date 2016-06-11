#' Function to get OpenStreetMap boundaries as spatial polygons. 
#' 
#' \code{get_osm_boundary} accesses \href{http://polygons.openstreetmap.fr/index.py}{this}
#' polygon creator tool. 
#' 
#' @param id A vector of OpenStreetMap relations. An integer key.
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
get_osm_boundary <- function(id, progress = "none") {
  
  # Vectorise function
  sp_list <- plyr::llply(id, osm_boundary_worker, .progress = progress)
  
  # Bind list
  sp <- sp_bind(sp_list)
  
  # Drop unique observation id
  sp@data$id <- NULL
  
  # Return
  sp
  
}

# No export
osm_boundary_worker <- function(id) {
  
  # Build query
  url <- stringr::str_c("http://polygons.openstreetmap.fr/get_wkt.py?id=", id, 
                        "&params=0")
  
  # Get wkt
  text <- readLines(url, warn = FALSE)
  
  # Drop preamble
  text <- stringr::str_replace(text, "SRID=4326;", "")
  
  # Promote to spatial
  sp <- sp_from_wkt(text, verbose = FALSE)
  
  # Add projection
  sp <- sp_transform(sp, warn = FALSE)
  
  # Give a data slot
  sp <- sp_promote(sp)
  
  # Add open street map id to data slot
  sp@data$osm_id <- id
  
  # Return
  sp
  
}
