#' Function to create WKT strings. 
#' 
#' @param latitude
#' 
#' @param longitude
#' 
#' @param type
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector.
#' 
#' @export
str_wkt <- function(latitude, longitude, type = "point") {
  
  # Parse
  type <- stringr::str_trim(type)
  type <- stringr::str_to_lower(type)
  
  if (type %in% c("point", "points")) 
    geom <- stringr::str_c("POINT (", longitude, " ", latitude, ")")
  
  return(geom)
  
}
