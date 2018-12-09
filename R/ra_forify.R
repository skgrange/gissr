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
  
  # Make good names
  if (dim(ra)[3] == 1) {
    
    # Give names
    names(df) <- c("value", "x", "y")
    
  } else {
    
    # Use variable names
    df <- tidyr::gather(df, variable, value, -c(x, y))
    
  }
  
  return(df)
  
}
