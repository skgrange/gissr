#' Function to transform a raster object to spatial polygons. 
#' 
#' @param ra Raster object. 
#' 
#' @param n Number of nodes which make up the polygon (4, 8, or 16). 
#' 
#' @param na.rm Should raster cells with \code{NA}s be omitted? 
#' 
#' @param digits Number of digits for the coordinates. 
#' 
#' @param dissolve Should polygons with the same values be dissolved into 
#' multi-polygon objects? 
#' 
#' @param data Should the data slot be dropped? 
#' 
#' @return Spatial polygons (SpatialPolygonsDataFrame or SpatialPolygons). 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{rasterToPolygons}}
#' 
#' @export
ra_to_polygons <- function(ra, n = 4, na.rm = TRUE, digits = 12, 
                           dissolve = FALSE, data = TRUE) {
  
  # Check
  stopifnot(is.ra(ra))
  
  # To spatial polygons
  sp <- raster::rasterToPolygons(
    ra, 
    n = n, 
    na.rm = na.rm, 
    digits = digits, 
    dissolve = dissolve
  )
  
  # Drop data slot
  if (!data) sp <- sp_demote(sp)
  
  return(sp)
  
}


#' @rdname ra_to_polygons
#'
#' @export
ra_to_points <- function(ra) {
  stopifnot(is.ra(ra))
  raster::rasterToPoints(ra, spatial = TRUE)
}
