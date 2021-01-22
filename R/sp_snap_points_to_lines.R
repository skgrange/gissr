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
#' @param rename Should the \code{id} variable be renamed? 
#' 
#' @return Spatial points with altered coordinates; all points will lie on lines
#' supplied for \code{sp_lines}. 
#' 
#' @seealso \link{snapPointsToLines}
#' 
#' @export
sp_snap_points_to_lines <- function(sp_points, sp_lines, max_distance = NA,
                                    id = NA, rename = TRUE) {
  
  # Message suppression is for projection warning in sp somewhere 
  sp <- suppressWarnings(
    maptools::snapPointsToLines(
      points = sp_points, 
      lines = sp_lines, 
      maxDist = max_distance, 
      idField = id
    )
  )
  
  # Rename the id variable
  if (!is.na(id[1]) && rename) {
    names(sp) <- if_else(names(sp) == "nearest_line_id", id, names(sp))
  }
  
  return(sp)
  
}
