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
#' @param id The variable in \code{sp_lines} which will be transferred to 
#' \code{sp_points} to distinguish the line which each point was snapped to. 
#' 
#' @return Spatial points with altered coordinates; all points will lie on lines
#' supplied for \code{sp_lines}. 
#' 
#' @seealso \link{snapPointsToLines}
#' 
#' @export
sp_snap_points_to_lines <- function(sp_points, sp_lines, max_distance = NA,
                                    id = NA) {
  
  maptools::snapPointsToLines(
    points = sp_points, 
    lines = sp_lines, 
    maxDist = max_distance, 
    withAttrs = TRUE,
    idField = id
  )
  
}
