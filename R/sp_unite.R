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
  
  # Lines
  if (grepl("lines", class(sp), ignore.case = TRUE)) 
    sp <- rgeos::gLineMerge(sp)
    
  # Polygons
  if (grepl("polygons", class(sp), ignore.case = TRUE)) 
    sp <- rgeos::gUnaryUnion(sp)
  
  # Return
  sp
  
}
