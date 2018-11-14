#' Function to wrap ggplot2::fortify which transforms geometries to data frames
#' (tables). 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object to be reformatted into tabular data. 
#' 
#' @param rename Should \code{lat} and \code{long} be renamed to \code{latitude} 
#' and \code{longitude}? 
#' 
#' @seealso \code{\link{fortify}}
#' 
#' @return Tibble. 
#' 
#' @export
sp_fortify <- function(sp, rename = TRUE) {
  
  # ggplot does not do points
  if (grepl("points", sp_class(sp), ignore.case = TRUE)) {
    
    # To data frame
    df <- data.frame(sp, stringsAsFactors = FALSE)
    
    # Drop optional if it exists, why is this here? 
    if (any(grepl("optional", names(df)))) df$optional <- NULL
    
    if (rename) {
      
      # Remove variables if they already exist, they will be overwritten
      if ("latitude" %in% names(df)) df$latitude <- NULL
      if ("longitude" %in% names(df)) df$longitude <- NULL
      
      # Rename variables
      names(df) <- ifelse(
        names(df) %in% c("x", "coords.x1", "X"), 
        "longitude", 
        names(df)
      )
      
      names(df) <- ifelse(
        names(df) %in% c("y", "coords.x2", "Y"),
        "latitude", 
        names(df)
      )
      
    }
    
  } else {
    
    # Create table, message suppression for polygons
    suppressMessages(
      df <- ggplot2::fortify(sp)
    )
    
    if (rename) {
      
      # Rename variables
      names(df) <- ifelse(names(df) == "lat", "latitude", names(df))
      names(df) <- ifelse(names(df) == "long", "longitude", names(df))
      
    }
    
  }
  
  # Arrange variables
  df <- select(df, latitude, longitude, everything())
  
  # To tibble
  df <- as_tibble(df)
  
  return(df)
  
}
