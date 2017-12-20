#' Function to unite polygons together and create a single spatial object, 
#' \emph{i.e.} disolve. 
#' 
#' If the individual polygons have data associated with them, it will be 
#' dropped when the polygons are united. 
#' 
#' @param sp Spatial polygons. 
#' 
#' @param features Should features within \code{sp} attempted to be preserved? 
#'
#' @author Stuart K. Grange
#' 
#' @return Spatial polygons. 
#'
#' @export
sp_dissolve_polygons <- function(sp, features = NULL) 
  rgeos::gUnaryUnion(sp, id = features)
