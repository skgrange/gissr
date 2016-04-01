#' Function to find a spatial object's length in metres or kilometres. 
#' 
#' \code{sp_length} is a wrapper for \code{rgeos::gLength} and calculates the 
#' lengths of spatial lines or polygons. If \code{sp_length} is used with 
#' polygons, it returns the length of the perimeter of the polygons. 
#' 
#' The trigonometric function, \code{geosphere::perimeter} performs poorly for 
#' length calculations. Therefore, when spatial objects are used with 
#' \code{sp_length} which have projection systems without metre units (such as 
#' WGS84), the Mollweide projection system is used by default. The Mollweide 
#' projection is applicable in any location on Earth, but the accuracy of the 
#' returned values is dependent on location.
#' 
#' If the spatial objects are located in a zone which has a more appropriate 
#' projection system, it is highly recommended that this is used. For example, 
#' spatial data in New Zealand should be projected in the New Zealand Transverse 
#' Mercator 2000 while data in the UK should be projected in British National 
#' Grid; both of which have metre units (\code{+units=m}).
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_length <- function(sp, unit = "m", features = TRUE) {
  
  # Check units argument
  if (!unit %in% c("m", "km")) stop("Unit must be 'm' or 'km'")
  
  # geosphere::perimeter has large errors for lat and long pairs, therefore, do
  # something different
  if (!grepl("+units=m", sp_projection(sp))) {
    
    # Transform projection to Mollweide projection/ESRI:54009, a worldwide 
    # projection with metre units
    sp <- sp_transform(sp, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # Give a message
    message("The projection system was temporarily transformed for calculation.")
    
  }
  
  # Calculate length
  suppressWarnings(
    vector <- rgeos::gLength(sp, byid = features)
  )
  
  # Drop names
  vector <- unname(vector)
  
  # Transform unit
  if (unit == "km") vector <- vector / 1000
  
  # Return
  vector
  
}
