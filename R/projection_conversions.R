#' Functions to convert points amoung different projection systems. 
#' 
#' These functions use spatial libraries to do the coordinate conversion so the
#' sometimes many-step calculations do not have to be maintained. 
#' 
#' @author Stuart K. Grange
#'
#' @export
#'
wgs84_to_osgb36 <- function (y, x) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to wgs84
  suppressMessages(
    sp <- sp_transform(sp)
  )
  
  # Do the conversion to osgb36
  sp.transformed <- sp_transform(sp, "bng")
  
  # Extract coordinates from spatial object
  coordinates <- sp.transformed@coords
  coordinates <- data.frame(x = coordinates[, 1], y = coordinates[, 2])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}

#' @rdname wgs84_to_osgb36
#' @export
#'
osgb36_to_wgs84 <- function (x, y) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to osgb36
  suppressMessages(
    sp <- sp_transform(sp, "bng")
  )
  
  # Do the conversion to wgs84
  sp.transformed <- sp_transform(sp)
  
  # Extract
  
  # Extract coordinates from spatial object
  coordinates <- sp.transformed@coords
  
  # Order is different for lat and longs
  coordinates <- data.frame(latitude = coordinates[, 2], longitude = coordinates[, 1])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}
