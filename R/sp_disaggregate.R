#' Function to disaggregate spatial objects.
#' 
#' \code{sp_disaggregate} will break apart multi-polygons to create many 
#' polygons and is a simple wrapper for \code{\link{sp::disaggregate}}.
#' 
#' @param sp Spatial object 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_disaggregate <- function(sp) disaggregate(sp)
