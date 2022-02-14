#' Function to 
#' 
#' @param df Data frame containing \code{latitude}, \code{longitude}, and 
#' \code{value} variables. The points also need to be on a regular grid. 
#' 
#' @param resolution Raster's resolution, in \code{projection}'s units. 
#' 
#' @param projection df's latitude and longitude projection system. Default is 
#' WGS84.
#' 
#' @param digits Precision to use for detecting if the points are on a 
#' rectangular grid. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster layer. 
#' 
#' @export
ra_from_xyz <- function(df, resolution = NA, projection = projection_wgs84(), 
                        digits = 5) {
  
  # Test input
  if (!all(c("latitude", "longitude", "value") %in% names(df))) {
    stop("Input must have `latitude`, `longitude`, and `value` variables.", call. = FALSE)
  }
  
  # Make sure the order is correct and pass to raster
  ra <- df %>% 
    select(longitude, 
           latitude,
           value) %>% 
    raster::rasterFromXYZ(res = resolution, crs = projection, digits = digits)
  
  return(ra)
  
}
