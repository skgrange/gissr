#' Function to convert map database objects to spatial data. 
#' 
#' @param map Map database to turn into spatial data. \code{map} can be either
#' \code{"map"} or \code{"world_hires"}. 
#' 
#' @return SpatialPolygonsDataFrame with \code{id} and \code{name} variables. 
#' 
#' @examples 
#' \dontrun{
#' # Lower resolution
#' sp_map <- map_to_sp(map = "world")
#' 
#' #
#' sp_map_better <- map_to_sp(map = "world_hires")
#' 
#' }
#' 
#' @author Stuart K. Grange
#'
#' @export
map_to_sp <- function(map = "world") {
  
  map <- stringr::str_to_lower(map)
  map <- stringr::str_replace(map, "world.hires|world_hires|world_highres|world_high", 
                              "worldHires")
  
  map_allowed <- c("world", "worldHires")
  
  if (!map %in% map_allowed) stop("'map' not recognised.", call. = FALSE)
  
  # Get polygons
  map_data <- maps::map(map, fill = TRUE, plot = FALSE)
  
  # Get id vector, uses names
  id <- sapply(strsplit(map_data$names, ":"), function(x) x[1])
  
  # Create polygons
  sp <- maptools::map2SpatialPolygons(map_data, IDs = id)
  
  # Give projection
  sp@proj4string <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Clean-up bad geometries
  suppressWarnings(
    sp <- sp_buffer(sp, features = TRUE, width = 0)
  )
  
  # Promote to spatial data frame
  sp <- sp_promote(sp)
  
  # Give another id
  sp@data$name <- sp_feature_ids(sp)
  
  # Return
  sp
  
}
