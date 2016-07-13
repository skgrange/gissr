#' Function to unite spatial objects. 
#' 
#' \code{sp_unite} will join/combine/unite geometries which intersect. 
#' 
#' This function is not complete. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @seealso \code{\link{gUnion}}
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
