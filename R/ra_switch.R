#' Function to switch a raster object's x coordinates from 0 to 360 to -180 and
#' 180 degrees.
#' 
#' @param ra Raster object. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @return Raster object.  
#' 
#' @seealso \code{\link[raster]{rotate}}
#' 
#' @export
ra_switch <- function(ra) {
  
  # Check input
  stopifnot(is.ra(ra))
  
  suppressWarnings(
    ra <- raster::rotate(ra)
  )
  
  return(ra)
  
}
