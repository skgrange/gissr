#' Function to create a rectangular polygon envelope, usually used for filtering. 
#' 
#' @param envelope A vector with the length of 4 containing the extent of the 
#' envelope to be used as a bounding box. The order is: xmin, xmax, ymin, ymax.
#' Watch out when using latitude and longitude, the order is longitude, 
#' longitude, then latitude, latitude.
#' 
#' @param projection A proj4 string for the projection-transformation. Default 
#' is the WGS84 string. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_create_envelope <- function(envelope, 
                               projection = "+proj=longlat +datum=WGS84 +no_defs") {
  
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
  polygon <- sp::SpatialPolygons(list(polygon))
  
  # Give projection
  if (!is.na(projection)) polygon <- sp_transform(polygon, projection, warn = FALSE)
  
  # Return
  polygon
  
}


#' Function to create a elliptical polygon from a point, usually used for 
#' filtering. 
#' 
#' @param latitude Latitude of a point. 
#'
#' @param longitude Longitude of a point. 
#'
#' @param width Width of radius. \code{"width"}'s unit is in the projection of
#' \code{projection}. The default is 0.01 decimal degrees. 
#'
#' @param projection A proj4 string. Default is the WGS84 string. 
#' 
#' @author Stuart K. Grange
#'
#' @export
sp_create_ellipse <- function(latitude, longitude, width = 0.01,
                              projection = "+proj=longlat +datum=WGS84 +no_defs") {
  
  # Create a point
  sp_point <- sp::SpatialPoints(cbind(longitude, latitude))
  
  # Give projection
  sp_point <- sp_transform(sp_point, projection, warn = FALSE)
  
  # Buffer point
  suppressWarnings(
    sp_polygon <- sp_buffer(sp_point, width = width)
  )
  
  # Return
  sp_polygon
  
}
