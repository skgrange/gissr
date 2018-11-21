#' Function to calculate focal values for a raster object using a matrix of 
#' weights.
#' 
#' \code{ra_focal} has the effect of smoothing a raster object. 
#' 
#' @param ra Raster object. 
#' 
#' @param weights A 3 by 3 matrix of values for the focal values. 
#' 
#' @param fun Function to use to sumamrise the focal values
#' 
#' @param na.rm Should \code{NA}s be ommited from the summaries? 
#' 
#' @param na.only Should only cells with \code{NA}s be replaced with focal 
#' values?
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster object. 
#' 
#' @examples
#' \dontrun{
#' 
#' ra_focal(
#'   ra_heights, 
#'   weights = matrix(1, 3, 3), 
#'   fun = mean, 
#'   na.rm = TRUE, 
#'   na.only = TRUE
#' )
#' 
#' } 
#' 
#' @export
ra_focal <- function(ra, weights, fun = mean, na.rm = FALSE, na.only = FALSE) {
  
  raster::focal(
    ra, 
    w = weights,
    fun = fun,
    na.rm = na.rm,
    NAonly = na.only
  )
  
}
