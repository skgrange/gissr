#' Function to extract a raster object's cell number for spatial points. 
#' 
#' @author Stuart K. Grange
#' 
#' @param ra A raster object. 
#' 
#' @param sp A \code{SpatialPointsDataFrame} object. 
#' 
#' @return \code{sp} with an extra \code{cell_raster} variable in the data slot.
#' 
#' @export
ra_cell_number <- function(ra, sp) {
  
  # Check inputs
  stopifnot(is.ra(ra))
  stopifnot(stringr::str_detect(sp_class(sp), "PointsData"))
  
  # Get raster cell numbers for each sp
  cell_raster <- raster::cellFromXY(ra, sp)
  
  # Add to spatial point data slot
  sp$cell_raster <- cell_raster
  
  return(sp)
  
}
