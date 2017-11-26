#' Function to create tessellation polygons from spatial points. 
#' 
#' Tessellation polygons have no gaps and the Dirichlet tessellation method is
#' used. 
#' 
#' @param sp Spatial points object. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_tessellation_polygons <- function(sp) {
  
  # Check
  if (!grepl("point", sp_class(sp), ignore.case = TRUE)) 
    stop("Input must be spatial points...", call. = FALSE)
  
  # Does the object have a data slot
  slot_data <- grepl("data", sp_class(sp), ignore.case = TRUE)
  
  # Create ppp object, there are other options here, but points only here
  if (slot_data) {
    
    sp_ppp <- maptools::as.ppp.SpatialPointsDataFrame(sp)
    
  } else {
    
    sp_ppp <- maptools::as.ppp.SpatialPoints(sp)
    
  }
  
  # Dirichlet tessellation of the ppp object
  sp_polygons <-  suppressMessages(spatstat::dirichlet(sp_ppp))
  
  # Make spatial polygons again
  sp_polygons <- as(sp_polygons, "SpatialPolygons")
  
  # Give projection
  sp_polygons <- sp_transform(sp_polygons, to = sp_projection(sp), warn = FALSE)
  
  # Add data slot too
  if (slot_data) {
    
    if (identical(nrow(sp_ppp$marks), length(sp_polygons))) {
      
      sp_polygons <- sp::SpatialPolygonsDataFrame(
        sp_polygons, 
        data = sp_ppp$marks, 
        match.ID = FALSE
      )
      
    } else {
      
      warning(
        "Data slot cannot be added to tessellation polygons...", 
        call. = FALSE
      )
      
    }
    
  }
  
  return(sp_polygons)
  
}
