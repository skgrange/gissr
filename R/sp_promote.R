#' Function to add a data slot in geometries. 
#' 
#' \code{sp_promote} will add a single variable named \code{id} to the data slot
#' of a geometry. \code{id} is a character vector. 
#' 
#' @param sp Spatial object
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Promote to spatial data frame
#' sp_with_data <- sp_promote(sp)
#' }
#'
#' @export
sp_promote <- function(sp) {
  
  # Get class of object
  sp_class <- class(sp)[1]
  
  if (!grepl("Data", sp_class)) {
    
    # Create id vector
    id_vector <- as.character(1:length(sp))
    
    # Points
    if (sp_class == "SpatialPoints") {
      
      sp <- sp::SpatialPointsDataFrame(sp, data = data.frame(id = id_vector),
                                       match.ID = FALSE)
    }
    
    # Lines
    if (sp_class == "SpatialLines") {
      
      sp <- sp::SpatialLinesDataFrame(sp, data = data.frame(id = id_vector),
                                      match.ID = FALSE)
    }
    
    # Polygons
    if (sp_class == "SpatialPolygons") {
      
      sp <- sp::SpatialPolygonsDataFrame(sp, data = data.frame(id = id_vector),
                                         match.ID = FALSE)
    }
    
  }
  
  # Return
  sp
  
}


#' Function to drop data slots in geometries. 
#' 
#' \code{sp_demote} will remove the data slot in geometries. 
#' 
#' @param sp Spatial object
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Drop data slot
#' sp_without_data <- sp_promote(sp_with_data)
#' }
#'
#' @export
sp_demote <- function(sp) {

  # Get class of object
  sp_class <- class(sp)[1]
  
  # If contains data slot, drop it
  if (grepl("Data", sp_class)) {
    
    # Points
    if (sp_class == "SpatialPointsDataFrame")
      sp <- as(sp, "SpatialPoints")

    # Lines
    if (sp_class == "SpatialLinesDataFrame")
      sp <- as(sp, "SpatialLines")
    
    # Polygons
    if (sp_class == "SpatialPolygonsDataFrame")
      sp <- as(sp, "SpatialPolygons")
    
  }
  
  # Return
  sp
  
}
