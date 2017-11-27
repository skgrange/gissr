#' Function to intersect/clip/crop/filter spatial objects by a spatial polygon. 
#' 
#' @param sp Spatial object to be clipped. 
#' 
#' @param sp_polygon Spatial polygon used as the filter. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Spatial data class, the same as \code{sp}.
#' 
#' @seealso \code{\link{sp_filter}}, \code{\link{sp_clip}}
#' 
#' @export
sp_intersect <- function(sp, sp_polygon) raster::intersect(sp, sp_polygon)
