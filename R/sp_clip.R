#' Function to clip spatial objects by a rectangular envelope.  
#' 
#' \code{sp_clip} uses \code{raster::crop} rather than 
#' \code{rgeos::gIntersection} by default because the \code{crop} method can 
#' keep the data slot in the spatial object intact at times.  
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object to be clipped. 
#' 
#' @param envelope A vector with the length of 4 containing the extent of the 
#' envelope to be used as a bounding box. The order is: xmin, xmax, ymin, ymax.
#' Watch out when using latitude and longitude, the order is longitude, longitude,
#' then latitude, latitude. These coordinates need to be in the same projection 
#' as \code{sp}. 
#' 
#' @param method What method to use? Default is to use \code{raster::crop} but
#' \code{rgeos::gIntersection} could be used if prefered. \code{method} can be
#' \code{"raster"} or \code{"rgeos"}
#' 
#' @seealso \code{\link{sp_filter}}, \code{\link{sp_intersect}}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Clip a spatial lines object which contains roads in Saudi Arabia
#' 
#' # Build envelope
#' envelope <- c(23.830562, 25.584393, 45.200111, 47.311495)
#' 
#' # Clip spatial lines
#' sp_road_clip <- sp_clip(sp_roads, envelope)
#' 
#' }
#' 
#' @export
sp_clip <- function(sp, envelope, method = "raster") {
  
  if (method == "raster") sp <- raster::crop(sp, envelope)
  
  if (method == "rgeos") {
    
    # Use rgeos to clip, faster, but will loose data slot
    # Promote
    sp_envelope <- as(raster::extent(envelope), "SpatialPolygons")
    
    # Give projection
    projection <- sp_projection(sp)
    sp_envelope <- sp_transform(sp_envelope, projection, warn = FALSE)
    
    # Clip
    sp <- rgeos::gIntersection(sp, sp_envelope, byid = TRUE)
    
  }
  
  return(sp)
  
}


#' @rdname sp_clip
#' 
#' @export
sp_crop <- sp_clip
