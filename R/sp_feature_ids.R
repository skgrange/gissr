#' Function to get ids from spatial objects.
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @export
sp_feature_ids <- function(sp) {
  
  # Points
  if (grepl("points", sp_class(sp), ignore.case = TRUE))
    id <- row.names(sp@data)
  
  # Lines
  if (grepl("line", sp_class(sp), ignore.case = TRUE)) {
    
    id <- sapply(slot(sp, "lines"), slot, "ID")
    
    # Stip names for lines
    id <- as.vector(id)
    
  }
  
  # Polygons
  if (grepl("polygon", sp_class(sp), ignore.case = TRUE))
    id <- sapply(slot(sp, "polygons"), slot, "ID")
  
  # Return
  id
  
}
