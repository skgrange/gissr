#' Function to create the smallest convex polygon that contains all input
#' geometries. 
#' 
#' \code{sp_convex_hull} is a simple wrapper for \code{rgeos::gConvexHull}. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param features Should the individual polygons of the features contained 
#' within \code{sp} be calculated? Default is \code{FALSE}.  
#' 
#' @examples 
#' \dontrun{
#' # Create a boundary-polygon containing all points
#' sp_boundary_polygon <- sp_convex_hull(sp_points)
#' }
#' 
#' @export
sp_convex_hull <- function(sp, features = FALSE)
  # Just a wrap
  rgeos::gConvexHull(sp, byid = features)
