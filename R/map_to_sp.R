#' Function to convert map database objects to spatial data. 
#' 
#' @author Stuart K. Grange
#'
#' @export
map_to_sp <- function(map = "world") {
  
  # Polygons
  map_data <- maps::map(map, fill = TRUE, plot = FALSE)
  
  # Get id vector, uses names
  id <- sapply(strsplit(map_data$names, ":"), function(x) x[1])
  
  # Create polygons
  sp <- maptools::map2SpatialPolygons(map_data, IDs = id)
  
  # Give projection
  sp <- sp_transform(sp, warn = FALSE)
  
  # Clean-up bad geometries
  suppressWarnings(
    sp <- sp_buffer(sp, features = TRUE, width = 0)
  )
  
  # Return
  sp
  
}
