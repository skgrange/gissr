#' Function to return a spatial object's projection system. 
#' 
#' @param sp Spatial object
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with length of 1. 
#' 
#' @export
sp_projection <- function(sp) {
  # Warning suppression is needed for a new rgdal version
  # `CRS object has comment, which is lost in output`
  suppressWarnings(proj4string(sp))
}
