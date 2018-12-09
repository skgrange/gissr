#' Function to extract bounding box from a spatial or raster object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial or raster object object. 
#' 
#' @return Numeric vector with length of four. Coordinates order is: xmin, xmax, 
#' ymin, ymax. 
#' 
#' @export
sp_bounding_box <- function(sp) {
  
  # Extents will be different type of objects for the different 
  if (is.sp(sp)) {
    
    # Get boundary
    x <- sp@bbox
    
    # Make a vector, xmin, xmax, ymin, ymax
    x <-  c(x[1, 1], x[1, 2], x[2, 1], x[2, 2])
    
  } else if (is.ra(sp)) {
    
    # Get boundary and make vector
    x <- raster::extent(sp)
    x <- as.vector(x)
    
  } else {
    
    stop("Data type not recognised...", call. = FALSE)
    
  }
  
  return(x)
  
}
