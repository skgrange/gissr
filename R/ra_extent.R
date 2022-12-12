#' Function to create a raster-extent object based on a spatial polygon. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp A spatial polygon.
#' 
#' @return A raster-extent object
#' 
#' @export
ra_extent <- function(sp) {
  stopifnot(stringr::str_detect(sp_class(sp), "Polygon"))
  raster::extent(sp)
}
