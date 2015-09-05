#' Function to clip spatial objects by a rectangular envelope.  
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object to be clipped. 
#' @param envelope A vector containing the extend of the envelope to be used as a
#' bounding box. The order must be: xmin, xmax, ymin, ymax and should be in the
#' same projection as \code{sp}. 
#' 
#' @seealso \link{`[`}
#' 
#' @export
#' 
sp_clip <- function (sp, envelope) {
  
  # Use rgeos to clip, faster, but will loose data slot
  # sp.clip <- rgeos::gIntersection(sp, sp.envelope, byid = TRUE)
  sp.clip <- raster::crop(sp, envelope)
  
  # Return
  sp.clip
  
}
