#' Function to convert a spatial object to a vector of WKT or a tibble 
#' containing a WKT variable. 
#' 
#' @param sp Spatial object to convert to WKT. 
#' 
#' @param features Should features within \code{sp} form different WKT strings?
#' 
#' @param data If \code{sp} contains a data slot, should a data frame be created
#' with a WKT variable? 
#' 
#' @param wkt Variable name of WKT strings in the returned data frame is 
#' \code{data} is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#'
#' @export
sp_to_wkt <- function(sp, features = TRUE, data = TRUE, wkt = "geom") {
  
  # If geomerty contains data
  if (grepl("data", sp_class(sp), ignore.case = TRUE)) {
    
    # Create wkt from geom
    text <- rgeos::writeWKT(sp, byid = TRUE)
    
    # Get data slot
    df <- sp@data
    
    # Add to df as a variable
    df[, wkt] <- text
    
    # To tibble
    df <- tibble::as_tibble(df)
    
  } else {
    
    # No data slot, just create wkt
    # Not a helpful name here
    df <- rgeos::writeWKT(sp, byid = features)
    
  }
  
  return(df)
  
}
