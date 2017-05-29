#' Function to create WKT strings. 
#' 
#' @param latitude Latitude vector. 
#' 
#' @param longitude Longitude vector. 
#' 
#' @param type Type of geom to create. Only \code{"points"} are supported 
#' currently. 
#' 
#' @param round Decimal points to round \code{latitude} and \code{longitude}
#' vector. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Character vector.
#' 
#' @export
str_wkt <- function(latitude, longitude, type = "point", round = NA) {
  
  # Parse argument
  type <- stringr::str_trim(type)
  type <- stringr::str_to_lower(type)
  
  # Round variables
  if (!is.na(round)) {
    
    latitude <- round(latitude, round)
    longitude <- round(longitude, round)
    
  }
  
  # Build the strings
  if (type %in% c("point", "points")) {
    
    geom <- stringr::str_c("POINT (", longitude, " ", latitude, ")")
    
  } else {
    
    warning("'type' not supported...", call. = FALSE)
    geom <- NA
    
  }
  
  return(geom)
  
}
