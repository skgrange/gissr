#' Function to clip spatial objects by a rectangular envelope.  
#' 
#' \code{sp_clip} uses \code{raster::crop} rather than 
#' \code{rgeos::gIntersection} because the \code{crop} method keeps the data slot 
#' in the spatial object intact. 
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
#' @examples 
#' 
#' \dontrun{
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
sp_clip <- function(sp, envelope) {
  
  # Use rgeos to clip, faster, but will loose data slot
  # sp.envelope <- as(raster::extent(envelope), "SpatialPolygons")
  # sp::proj4string(sp.envelope) <- sp::CRS(proj4string(sp))
  # sp.clip <- rgeos::gIntersection(sp, sp.envelope, byid = TRUE)
  
  sp <- raster::crop(sp, envelope)
  
  # Return
  sp
  
}
