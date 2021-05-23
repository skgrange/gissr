#' Function to calculate areas of a spatial object and add the results to the 
#' data slot. 
#' 
#' @param sp A spatial polygons object with a data slot.  
#' 
#' @param round Number of digits to round the calculated area result to. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Spatial polygons with an additional variable in the data slot. 
#' 
#' @export
sp_add_area <- function(sp, round = NA) {
  
  # Check for data slot
  stopifnot(stringr::str_detect(class(sp), "Data"))
  
  # Calculate areas
  x <- sp_area(sp, features = TRUE)
  
  # Round if desired
  if (!is.na(round)) x <- round(x, round)
  
  # Add to data slot
  sp$area <- x
  
  return(sp)
  
}
