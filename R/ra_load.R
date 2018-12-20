#' Function to explicitly load a raster object into physical memory. 
#' 
#' @param ra Raster object. 
#' 
#' @return Raster object.
#' 
#' @export
ra_load <- function(ra) {
  
  stopifnot(is.ra(ra))
  ra <- raster::readAll(ra)
  return(ra)
  
}
