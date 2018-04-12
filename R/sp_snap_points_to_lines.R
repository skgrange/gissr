#' Function to "snap" spatial points to the nearest spatial line.
#' 
#' @author Stuart K. Grange
#' 
#' @param sp_points Spatial points to be snapped. 
#' 
#' @param sp_lines Spatial lines to snap \code{sp_points} to. 
#' 
#' @param max_distance The maximum distance to avoid snapping points that are 
#' farther apart.
#' 
#' @return Spatial points with altered coordinates; all points will lie on lines
#' supplied for \code{sp_lines}. 
#' 
#' @export
sp_snap_points_to_lines <- function(sp_points, sp_lines, max_distance = NA)
  maptools::snapPointsToLines(sp_points, sp_lines, maxDist = max_distance)
