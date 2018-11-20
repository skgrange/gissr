#' Function to transform a raster object to a data frame (a table). 
#' 
#' @param ra Raster object. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
ra_forify <- function(ra) {
  
  # To vector object
  sp <- as(ra, "SpatialPixelsDataFrame")
  
  # To data frame
  df <- data.frame(sp)
  
  # To tibble
  df <- as_tibble(df)
  
  # Give names, to-do enhance
  names(df) <- c("value", "x", "y")
  
  return(df)
  
}
