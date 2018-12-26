#' Function to read a raster object from file. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file File containing raster data. 
#' 
#' @param in_memory Should the raster object be forced into physical memory? 
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link[raster]{raster}}
#' 
#' @export
ra_read <- function(file, in_memory = FALSE) {
  
  # Connect to raster object
  ra <- raster::raster(file)
  
  # Ensure raster object is in memory
  if (in_memory) ra <- ra_load(ra)
  
  return(ra)
  
}
