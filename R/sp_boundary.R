#' Function to get boundary of a spatial object. 
#' 
#' \code{sp_boundary} often requires a single object to determine a boundary. 
#' Use \code{\link{sp_unite}} to create a single geometry from many. 
#' 
#' @param sp Spatial object. 
#' 
#' @param features Create boundaries for features within \code{sp}. Default is
#' \code{FALSE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_boundary <- function(sp, features = FALSE) {
  
  # 
  # if (length(sp) != 1) sp <- sp_unite(sp)
  
  # 
  sp <- rgeos::gBoundary(sp, byid = features)
  
  # Return
  sp
  
}
