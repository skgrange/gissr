#' Function to make a gridded spatial points object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial polygons. 
#' 
#' @param cellsize Resolution of spatial grid in the projection system of 
#' \code{sp}. 
#' 
#' @return Spatial points. 
#'
#' @export
sp_make_grid <- function(sp, cellsize) {
  
  # Save projection string
  projection <- sp_projection(sp)
  
  # Make the grid
  df <- makegrid(sp, cellsize = cellsize) 
  
  # Promote
  sp <- SpatialPoints(df)
  
  # Give projection
  if (!is.na(projection)) sp <- sp_transform(sp, projection, warn = FALSE)
  
  return(sp)
  
}
