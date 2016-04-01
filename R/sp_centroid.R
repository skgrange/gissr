#' Function to find the centroid of a spatial object. 
#'
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' @param features Should the centroids of all features within \code{sp} be
#' calculated? Default is \code{TRUE}. 
#'
#' @export
sp_centroid <- function(sp, features = TRUE) 
  rgeos::gCentroid(sp, byid = features)
