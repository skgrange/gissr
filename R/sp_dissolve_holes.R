#' Function to dissolve/drop holes in a spatial polygon object.  
#' 
#' \code{sp_dissolve_holes} is useful when polygons contain holes but only the 
#' outer boundary is desired. 
#' 
#' If the individual polygons have data associated with them, it will be 
#' dropped when the holes are dissolved. 
#' 
#' @param sp Spatial polygon object. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Spatial polygon. 
#'
#' @export
sp_dissolve_holes <- function(sp) {
  
  # Check
  if (!grepl("polygon", sp_class(sp), ignore.case = TRUE)) {
    stop("sp must be a polygon.", call. = FALSE)
  }
  
  # Store projection string
  projection <- sp_projection(sp)
  
  # http://stackoverflow.com/questions/12663263/dissolve-holes-in-polygon-in-r
  list_outer <- Filter(
    function(f) { 
      f@ringDir == 1
    }, 
    sp@polygons[[1]]@Polygons
  )
  
  # Promote
  sp <- SpatialPolygons(list(Polygons(list_outer, ID = 1)))
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  return(sp)
  
}
