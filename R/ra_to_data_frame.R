#' Function to coerce a raster object into a data frame. 
#' 
#' @param ra Raster object. 
#' 
#' @param rename Should the variables in the returned data frame be altered? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
ra_to_data_frame <- function(ra, rename = FALSE) {
  
  # To data frame
  df <- data.frame(raster::rasterToPoints(ra, spatial = FALSE))
  
  # Reorder and rename variables
  if (rename) {
    
    # Reorder variables 
    df <- select(df, y, x, everything())
    
    # Rename
    df <- rename(df, latitude = y, longitude = x)
    
  }
  
  return(df)
  
}
