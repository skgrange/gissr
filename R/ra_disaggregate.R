#' Function to create a higher resolution ("disaggregate") raster layer. 
#' 
#' @param ra Raster object. 
#' 
#' @param factor Amount of disaggregation in number of cells or as a multiplier 
#' to increase \code{ra}'s current resolution. 
#' 
#' @param method Method to use for interpolation. 
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link{disaggregate}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Increase a raster layer's resolution by five times
#' ra_countries_smooth <- ra_disaggregate(ra_countries, factor = 5)
#' 
#' }
#' 
#' @export
ra_disaggregate <- function(ra, factor, method = "bilinear")
  raster::disaggregate(ra, fact = factor, method = method)
