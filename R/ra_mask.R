#' Function to mask/filter/crop a raster object to a spatial polygon. 
#' 
#' @param ra Raster object to be masked/filtered. 
#' 
#' @param sp_polygon Spatial polygon to be used as the filter. 
#' 
#' @param inverse Should an inverse/punch/erase mask be conducted? 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Raster object.
#' 
#' @export
ra_mask <- function(ra, sp_polygon, inverse = FALSE) 
  raster::mask(ra, sp_polygon, inverse = inverse)
