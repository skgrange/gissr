#' Function to unite polygons together and create a single spatial object. 
#' 
#' If the individual polygons have data associated with them, it will be 
#' dropped when the polygons are united. 
#'
#' @author Stuart K. Grange
#'
#' @export
#' 
sp_dissolve_polygons <- function (sp) {
  
  # Create a single sp object
  sp <- rgeos::gUnaryUnion(sp)
  
  # Return
  sp
  
}
