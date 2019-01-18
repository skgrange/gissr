#' Function to clip spatial (or raster) objects by a rectangular envelope or a 
#' spatial polygon.  
#' 
#' \code{sp_clip} uses \code{raster::crop} by default if an envelope is used 
#' rather than \code{rgeos::gIntersection} because the \code{crop} method can 
#' keep the data slot in the spatial object intact (at times).
#' 
#' If a spatial polygon is supplied, the \code{rgeos::gIntersection} method will
#' be used. 
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
#' @param sp_polygon A spatial polygon to use to clip \code{sp}. The projection 
#' systems of \code{sp} and \code{sp_polygon} must be identical. If this 
#' argument is used, the \code{method} argument will not be used. 
#' 
#' @param method What method to use? Default is to use \code{raster::crop} but
#' \code{rgeos::gIntersection} could be used if preferred. \code{method} can be
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
#' 
#' # Or use a polygon to clip a road network
#' sp_roads_clipped <- sp_clip(sp_roads_main, sp_gibraltar_peninsula)
#' 
#' }
#' 
#' @export
sp_clip <- function(sp, envelope, sp_polygon = NULL, method = "raster") {
  
  # Check input
  method <- stringr::str_to_lower(method)
  
  # If polygon supplied, use rgeos
  if (!is.null(sp_polygon[1])) method <- "rgeos"
  
  if (method == "raster") sp <- raster::crop(sp, envelope)
  
  # Use rgeos to clip, faster, but will loose data slot
  if (method == "rgeos") {
    
    if (is.null(sp_polygon[1])) {
      
      # Use the supplied envelope
      
      # Promote
      sp_envelope <- as(raster::extent(envelope), "SpatialPolygons")
      
      # Give projection
      projection <- sp_projection(sp)
      sp_envelope <- sp_transform(sp_envelope, projection, warn = FALSE)
      
      # Clip
      sp <- rgeos::gIntersection(sp, sp_envelope, byid = TRUE)
      
    } else {
      
      # Use supplied spatial polygon
      
      # Check the projection systems
      if (!identical(sp_projection(sp), sp_projection(sp_polygon)))
        stop("Projection systems are not identical...", call. = FALSE)
      
      # Do, this will drop data slot if it exists
      sp <- rgeos::gIntersection(sp, sp_polygon, byid = TRUE, drop_lower_td = TRUE)
      
    }
    
  }
  
  return(sp)
  
}


#' @rdname sp_clip
#' 
#' @export
sp_crop <- sp_clip


#' @rdname sp_clip
#' 
#' @export
ra_crop <- sp_clip
