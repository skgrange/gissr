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
  
  # Check input
  stopifnot(is.ra(ra))
  
  # To vector object
  sp <- as(ra, "SpatialPixelsDataFrame")
  
  # To tibble
  df <- sp %>% 
    data.frame() %>% 
    as_tibble()
  
  # Make good names
  if (dim(ra)[3] == 1) {
    # Give names
    names(df) <- c("value", "x", "y")
  } else {
    # Use variable names and make longer
    df <- tidyr::pivot_longer(df, -c(x, y), names_to = "variable")
  }
  
  return(df)
  
}
