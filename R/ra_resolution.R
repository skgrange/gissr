#' Function to get the resolution of a raster object.
#' 
#' @param ra Raster object.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
ra_resolution <- function(ra) {
  
  # Check input
  stopifnot(is.ra(ra))
  
  # Get resolution
  resolution <- raster::res(ra)
  
  # Is the projection system metric?
  metric <- ra %>% 
    sp_projection() %>% 
    stringr::str_detect("units=m")
  
  # Build tibble return
  df <- tibble(
    x = resolution[1], 
    y = resolution[2],
    metric = metric
  )
  
  return(df)
  
}
