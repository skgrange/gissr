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
                              projection = projection_wgs84()) {
  
  # Create a point
  sp_point <- sp::SpatialPoints(cbind(longitude, latitude))
  
  # Give projection
  sp_point <- sp_transform(sp_point, projection, warn = FALSE)
  
  # Buffer point
  suppressWarnings(
    sp_polygon <- sp_buffer(sp_point, width = width)
  )
  
  return(sp_polygon)
  
}
