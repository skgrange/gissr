#' Function to include or exclude an area of a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param features Should the geometries of all features within \code{sp} be
#' buffered? Default is \code{TRUE}. 
#' 
#' @param width Width of buffer. \code{width}'s units is in \code{sp}'s 
#' projection system.
#' 
#' @export
sp_buffer <- function(sp, features = TRUE, width)
  rgeos::gBuffer(sp, byid = features, width = width)
