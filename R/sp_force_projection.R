#' Function to force a spatial object's projection system without transformation. 
#' 
#' @param sp Spatial object which is to be manipulated. 
#' 
#' @param projection A proj4 string for the projection-transformation. Default 
#' is the WGS84 string. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Spatial object, the same type as \code{sp}. 
#' 
#' @export
sp_force_projection <- function(sp, projection = projection_wgs84()) {
  
  # Check type
  stopifnot(is.sp(sp))
  
  # Force
  raster::crs(sp) <- projection
  
  return(sp)
  
}
