#' Function to create a rectangular polygon envelope, usually used for filtering. 
#' 
#' @param envelope A vector with the length of 4 containing the extent of the 
#' envelope to be used as a bounding box. The order is: xmin, xmax, ymin, ymax.
#' Watch out when using latitude and longitude, the order is longitude, longitude,
#' then latitude, latitude.
#' 
#' @param projection A proj4 string for the projection-transformation. Default is 
#' the WGS84 string. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_create_envelope <- function (envelope, projection = "+proj=longlat +datum=WGS84 +no_defs") {
  
  # Could this use raster::extent? 
  
  # Create matrix from vector; the order matters
  matrix <- matrix(c(envelope[1], envelope[3], 
                     envelope[1], envelope[4],
                     envelope[2], envelope[4], 
                     envelope[2], envelope[3]), ncol = 2, byrow = TRUE)
  
  # Promote matrix to spatial data
  polygon <- Polygon(matrix)
  polygon <- Polygons(list(polygon), ID = "1")
  
  # Spatial class with projection
  polygon <- SpatialPolygons(list(polygon))
  
  # Give projection
  if (!is.na(projection)) {
    polygon <- sp_transform(polygon, projection, warn = FALSE)
  }
  
  # Return
  polygon
  
}
