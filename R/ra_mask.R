#' Function to mask/filter/crop a raster object to a spatial polygon. 
#' 
#' @param ra Raster object to be masked/filtered. 
#' 
#' @param sp_polygon Spatial polygon to be used as the filter. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Raster object. 
#' 
#' @export
ra_mask <- function(ra, sp_polygon) raster::mask(ra, sp_polygon)
