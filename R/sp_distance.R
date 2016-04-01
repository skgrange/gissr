#' Function to find distances between two spatial objects in metres or 
#' kilometres. 
#' 
#' \code{sp_distance} calculates the distance between a spatial object and 
#' another spatial object. An example of usage is when an object containing 
#' locations of cities is tested against coastline information to determine how
#' far cities are away from the coast.
#' 
#' \code{sp_distance} uses \code{rgeos::gDistance} for the distance calculations.
#' This function returns distances based on the unit of the projection system 
#' contained within the spatial objects. When spatial objects are used with 
#' \code{sp_distance} which have projection systems without metre units (such as 
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
#' \code{sp_distance} supports parallel processing by forking the 
#' \code{rgeos::gDistance} function across multiple cores. This can make the 
#' distance calculations faster.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{gDistance}}, \code{\link{sp_transform}}, 
#' \code{\link{mclapply}}
#' 
#' @param sp_1 Spatial object one. 
#' 
#' @param sp_2 Spatial object two. 
#' 
#' @param cores Number of cores for the function to use. Not available for 
#' Windows systems. 
#' 
#' @param unit If \code{"km"}, the returned vector is returned in kilometres 
#' rather than metres. 
#' 
#' @examples
#' \dontrun{
#' # Simple usage
#' # Calculate the distances of places from the coastline
#' distances <- sp_distance(sp_places, sp_coast_line, unit = "km")
#' 
#' # Speed the function up by using multiple system cores
#' distances <- sp_distance(sp_places, sp_coast_line, unit = "km", cores = 4)
#' 
#' 
#' # Usage for transforming a data frame
#' # Load shapefiles
#' # Coastline, spatial lines
#' sp_coast_line <- sp_read("coastlines/coastlines")
#' 
#' # Places in the UK, spatial points
#' sp_places <- sp_read("great-britain/places")
#' 
#' # Get a data frame from the sp_places object
#' data_places <- sp_places@data
#' 
#' # Find distances between every place in sp_places and sp_coast_line
#' # We are in the UK, therefore the British National Grid has been used as the
#' # projection system
#'
#' # Apply parallel function and add variable to data frame
#' data_places$distance <- sp_distance(sp_places, sp_coast_line, unit = "km", 
#'                                     cores = 4)
#'
#' # Have a look
#' head(data_places)
#' 
#' London 33.4
#' Basingstoke 43.5
#' York 37
#' Tobermory 0.22
#' Charlbury 60.3
#' 
#' # London will be dependent on where the coastline is set after the River
#' # Thames's mouth. 
#' }
#' 
#' @export
sp_distance <- function(sp_1, sp_2, cores = 1, unit = "m") {
  
  # Check the projection systems
  if (!identical(sp_projection(sp_1), sp_projection(sp_2)))
    stop("The projection systems of the two spatial objects are not identical.")
  
  # Transform projections to Mollweide projection/ESRI:54009 if projection systems
  # do not have metres for units
  if (!grepl("+units=m", sp_projection(sp_1))) {
    
    # Projection string
    projection <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
    
    # Do the transforming
    sp_1 <- sp_transform(sp_1, projection, warn = FALSE)
    sp_2 <- sp_transform(sp_2, projection, warn = FALSE)
    
    # Give a message
    message("The projection systems have been transformed for calculation.")
    
    # Need to degrade spatial-data-frame to just spatial for rgeos::gDistance
    if (grepl("data", class(sp_1), ignore.case = TRUE)) {
      
      # Loss of projection so need to state it again
      sp_1 <- sp::SpatialPoints(sp_1, CRS(projection))
      
    }
    
  }
  
  # Do the test
  if (cores == 1) {
    
    # Superscript notation is necessary
    distance <- lapply(1:length(sp_1), function(x) 
      rgeos::gDistance(sp_1[x], sp_2))
    
    # Make vector
    distance <- unlist(distance)
    
  } else {
    
    # Superscript notation is necessary
    distance <- parallel::mclapply(1:length(sp_1), function(x) 
      rgeos::gDistance(sp_1[x], sp_2), mc.cores = getOption("mc.cores", cores))
    
    # Make vector
    distance <- unlist(distance)
    
  }
  
  # Transform units
  if (unit == "km") distance <- distance / 1000
  
  # Return
  distance
  
}
