#' Function to interpolate a raster object. 
#' 
#' @param ra Raster object to be interpolated. 
#' 
#' @param variable Variable name to interpolate in \code{ra}. 
#' 
#' @param sp Spatial point object which contains the locations of the raster 
#' points. 
#' 
#' @param method Interpolation method to use. Only 
#' \code{inverse_distance_weighted} is implemented. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster layer. 
#' 
#' @export
ra_interpolate <- function(ra, variable = "value", sp, 
                           method = "inverse_distance_weighted") {
  
  # Build formula 
  formula <- stringr::str_c(variable, " ~ 1")
  formula <- as.formula(formula)
  
  # Inverse distance weighted interpolation
  fit <- gstat::gstat(formula = formula, locations = sp)
  
  # Interpolate surface
  ra <- threadr::quiet(raster::interpolate(ra, fit))
  
  return(ra)
  
}
