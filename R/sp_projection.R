#' Function to return a spatial object's projection system. 
#' 
#' @param sp Spatial object
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector with length of 1. 
#' 
#' @export
sp_projection <- function(sp) proj4string(sp)
