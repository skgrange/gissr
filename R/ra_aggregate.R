#' Function to create a lower resolution (aggregated) raster layer. 
#' 
#' @param ra Raster object. 
#' 
#' @param factor Amount of aggregatation in number of cells or as a multiplier 
#' to decrease \code{ra}'s current resolution. 
#' 
#' @param fun Summary function to for aggregatation.
#' 
#' @param na.rm Should \code{NA}s be omited from the aggregatation? 
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link{ra_disaggregate}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Increase a raster layer's resolution by five times
#' ra_countries_rough <- ra_aggregate(ra_countries, factor = 5)
#' 
#' }
#' 
#' @export
ra_aggregate <- function(ra, factor, fun = mean, na.rm = TRUE)
  raster::aggregate(ra, fact = factor, fun = fun, na.rm = na.rm)