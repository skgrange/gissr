#' Function to create a higher resolution ("disaggregate") raster layer. 
#' 
#' @author Stuart K. Grange
#' 
#' @param ra Raster object. 
#' 
#' @param factor Amount of disaggregation in number of cells. 
#' 
#' @param method Method to use for interpolation. 
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link{disaggregate}}
#' 
#' @examples 
#' \dontrun{
#' 
#' ra_countries_smooth <- ra_disaggregate(ra_countries, factor = 5)
#' 
#' }
#' 
#' @export
ra_disaggregate <- function(ra, factor, method = "bilinear")
  raster::disaggregate(ra, fact = factor, method = method)
