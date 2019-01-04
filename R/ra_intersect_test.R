#' Function to test if a raster object intersects with another spatial or raster
#' object. 
#' 
#' @param ra Raster object.
#' 
#' @param sp Spatial or raster object. 
#' 
#' @return Logical vector with length of 1. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
ra_intersect_test <- function(ra, sp) {
  
  # Check extent, if this returns something then true
  extent <- raster::intersect(raster::extent(ra), sp)
  
  if (is.null(extent)) {
    
    x <- FALSE 
    
  } else {
    
    x <- TRUE
    
  }
  
  return(x)
  
}
