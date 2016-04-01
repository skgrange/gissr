#' Function to unite polygons together and create a single spatial object, 
#' \emph{i.e.} disolve. 
#' 
#' If the individual polygons have data associated with them, it will be 
#' dropped when the polygons are united. 
#'
#' @author Stuart K. Grange
#'
#' @export
sp_dissolve_polygons <- function(sp, feature = NULL) 
  rgeos::gUnaryUnion(sp, id = feature)
