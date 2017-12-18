#' Function to test if a spatial object is valid. 
#' 
#' @param sp Spatial object. 
#' 
#' @param features Should all the sub-geometries be tested? 
#' 
#' @return Logical vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_is_valid <- function(sp, features = TRUE) {
  
  # Test
  x <- rgeos::gIsValid(sp, byid = features)
  x <- unname(x)
  return(x)
  
}
