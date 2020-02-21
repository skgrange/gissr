#' Function to unite spatial objects.
#' 
#' \code{sp_unite} will join/combine/unite geometries which are contained in the
#' same data object. For example, use \code{sp_unite} when a data object contains
#' many individual polygons but they form the same unit and therefore they are 
#' considered sub-geometries. 
#' 
#' For binding different spatial objects together, use \code{\link{sp_bind}}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @seealso \code{\link{gUnion}}, \code{\link{sp_bind}}
#'
#' @export
sp_unite <- function(sp) {
  
  # Unite objects
  if (stringr::str_detect(sp_class(sp), "Lines")) {
    sp <- rgeos::gLineMerge(sp)
  } else if (stringr::str_detect(sp_class(sp), "Polygons")) {
    sp <- rgeos::gUnaryUnion(sp)
  }
  
  return(sp)
  
}
