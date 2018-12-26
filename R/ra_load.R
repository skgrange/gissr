#' Function to explicitly load a raster object into physical memory. 
#' 
#' @param ra Raster object. 
#' 
#' @return Raster object.
#' 
#' @export
ra_load <- function(ra) {
  
  # Check object
  stopifnot(is.ra(ra))
  
  if (ra@data@inmemory) {
    
    message("Raster object already in memory...")
    
  } else {
    
    ra <- raster::readAll(ra)
    
  }
  
  return(ra)
  
}
