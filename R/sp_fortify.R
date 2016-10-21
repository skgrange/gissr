#' Function to wrap ggplot2::fortify which transforms geometries to data frames
#' (tables). 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param rename Should \code{lat} and \code{long} be renamed to \code{latitude} 
#' and \code{longitude}? Default is \code{TRUE}. 
#' 
#' @seealso \code{\link{fortify}}
#' 
#' @export
sp_fortify <- function(sp, rename = TRUE) {

  # Create table, message suppression for polygons
  suppressMessages(
    df <- ggplot2::fortify(sp)
  )
  
  if (rename) {
    
    # Rename variables
    names(df) <- ifelse(names(df) == "lat", "latitude", names(df))
    names(df) <- ifelse(names(df) == "long", "longitude", names(df))
    
    # Arrange variables
    df <- threadr::arrange_left(df, c("latitude", "longitude"))
    
  }
  
  # Return
  df
  
}
