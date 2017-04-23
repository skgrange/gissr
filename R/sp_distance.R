#' Function to find distances between two spatial objects. 
#' 
#' \code{sp_distance} calculates the distance between a spatial object and 
#' another spatial object. An example of usage is when an object containing 
#' locations of cities is tested against coastline information to determine how
#' far cities are away from the coast.
#' 
#' \code{sp_distance} uses \code{rgeos::gDistance} for the distance calculations.
#' This function returns distances based on the unit of the projection system 
#' contained within the spatial objects and both geometries' projection must be
#' identical. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{gDistance}}, \code{\link{sp_transform}}
#' 
#' @param sp_1 Spatial object one. 
#' 
#' @param sp_2 Spatial object two. 
#' 
#' @param unit If \code{"km"}, the returned vector is returned in kilometres 
#' rather than metres. 
#' 
#' @return Numeric vector or matrix.
#' 
#' @examples
#' \dontrun{
#' 
#' # Simple usage
#' # Calculate the distances of places from the coastline
#' distances <- sp_distance(sp_places, sp_coast_line, unit = "km")
#' 
#' # Speed the function up by using multiple system cores
#' distances <- sp_distance(sp_places, sp_coast_line, unit = "km")
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
#' data_places$distance <- sp_distance(sp_places, sp_coast_line, unit = "km")
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
#' 
#' }
#' 
#' @export
sp_distance <- function(sp_1, sp_2, features = FALSE, unit = "m") {
  
  # Check the projection systems
  if (!identical(sp_projection(sp_1), sp_projection(sp_2)))
    stop("Projection systems are not identical...", call. = FALSE)
  
  # Do the test
  x <- rgeos::gDistance(sp_1, sp_2, byid = features)
  
  # Transform units
  if (unit == "km") x <- x / 1000
  
  # Return
  x
  
}
