#' Function 
#' 
#' @param ra Raster object to be masked/filtered. 
#' 
#' @param sp_polygon Spatial polygon to be used as the filter. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
ra_mask <- function(ra, sp_polygon) raster::mask(ra, sp_polygon)
