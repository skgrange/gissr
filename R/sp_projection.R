#' Function to return a spatial object's projection system. 
#' 
#' @param sp Spatial object
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_projection <- function(sp) sp::proj4string(sp)
