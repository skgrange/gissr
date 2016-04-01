#' Convenience function to transform a spatial object's projection system to 
#' WGS84 latitude and longitude. 
#' 
#' \code{sp_transform} is a simple wrapper for \code{sp::spTransform} which has 
#' been written so spatial objects can be transformed quickly without the need 
#' to remember the WGS84 proj4 string. \code{sp_transform} will force projections
#' when the spatial object contains no projection information. 
#' 
#' @param sp Spatial object which is to be transformed.
#' 
#' @param to A proj4 string for the projection-transformation. Default is the 
#' WGS84 string. 
#' 
#' @param warn Should the functions raise a warning when the projection is forced?
#' 
#' @seealso \code{\link{spTransform}}, \code{\link{sp_projection}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Load a shape file of canal locks for the UK
#' sp_locks <- readOGR("uk-canals", "locks")
#' 
#' # Print projection string
#' sp_projection(sp_locks)
#' "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"
#' 
#' # Convert the shape file's projection (UK's Ordnance Survey National Grid)
#' to WGS84 latitude and longitude
#' sp_locks <- sp_transform(sp_locks)
#' 
#' # Print projection string
#' sp_projection(sp_locks)
#' "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#' }
#' 
#' @import sp
#' 
#' @export
sp_transform <- function(sp, to = "+proj=longlat +datum=WGS84 +no_defs",
                         warn = TRUE) {
  
  # Switch
  to <- ifelse(to %in% c("bng", "ogb", "osgb36"), "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs", to)
  to <- ifelse(to %in% c("nztm"), "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs", to)
  
  # If no projection
  if (is.na(proj4string(sp))) {
    
    # Warn
    if (warn) {
      
      warning("Spatial object had no projection. The projection has been forced.",
              call. = FALSE)
      
    }
    
    # Now force
    proj4string(sp) <- to
    
  } else {
    
    # Otherwise convert projection system
    sp <- spTransform(sp, CRS(to))
    
  }
  
  # Return
  sp
  
}
