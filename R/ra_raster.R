#' Function to create a raster object from either a raster-extent or spatial
#' object. 
#' 
#' @param x Raster-extent or spatial object. 
#' 
#' @param projection \code{x}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param resolution A vector with the length of 1 or 2 indicating the 
#' resolution of the raster object, in the units of \code{projection}. 
#' 
#' @param values Values for the raster object to have. This vector is passed to
#' \code{\link{ra_set_values}}. 
#' 
#' @param ra Raster object.
#' 
#' @export
ra_raster <- function(x, projection = projection_wgs84(), resolution = NA, 
                      values = NA) {
  
  # inherits(a, "Extent")
  
  # Create raster, bit of replication here, should to be dropped at some point
  if (!is.na(resolution)) {
    ra <- raster::raster(x, crs = projection, resolution = resolution)
  } else {
    ra <- raster::raster(x, crs = projection)
  }
  
  # Add values to raster
  if (!is.na(values[1])) {
    ra <- ra_set_values(ra, values = values)
  }
  
  return(ra)
  
}


#' @rdname ra_raster
#'
#' @export
ra_set_values <- function(ra, values) {
  raster::setValues(ra, values = values)
}

