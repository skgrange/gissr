#' Function to split a spatial object by feature. 
#' 
#' @param sp Spatial object. 
#'
#' @author Stuart K. Grange
#' 
#' @return Named list containing \emph{n} spatial objects.
#'
#' @export
sp_split <- function(sp) {
  
  # Split into a list containing individual features
  list_sp <- split(sp, 1:length(sp))
  
  # Check for other spatial data types too...
  
  return(list_sp)
  
}
