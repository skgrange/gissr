#' Function to convert the map database object to spatial polygons.  
#' 
#' @examples 
#' 
#' # Import the mapping data as spatial polyogons
#' sp_map <- map_to_sp()
#' 
#' @author Stuart K. Grange
#' 
#' @return SpatialPolygonsDataFrame.
#'
#' @export
map_to_sp <- function() {
  
  # Get polygons
  map_data <- maps::map("world", fill = TRUE, plot = FALSE)
  
  # Get id vector, uses names
  id <- sapply(strsplit(map_data$names, ":"), function(x) x[1])
  
  # Create polygons
  sp <- maptools::map2SpatialPolygons(map_data, IDs = id)
  
  # Give projection
  sp@proj4string <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Clean-up bad geometries
  suppressWarnings(
    sp <- sp_buffer(sp, features = TRUE, width = 0)
  )
  
  # Promote to spatial data frame
  sp <- sp_promote(sp)
  
  # Give another id
  sp@data$name <- sp_feature_ids(sp)
  
  return(sp)
  
}
