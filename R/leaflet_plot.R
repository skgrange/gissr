#' Convenience function to quickly plot geometries on a leaflet map. 
#' 
#' @param sp Spatial object to be plotted. 
#'
#' @param popup Popup variable to add to map. 
#'
#' @param force Should the projection be forced to WGS84? Default is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @export
leaflet_plot <- function (sp, popup = NULL, force = TRUE) {
  
  # Projection check
  # sp_projection(sp)
  if (force) sp <- sp_transform(sp)
  
  # Create map
  map <- leaflet(sp) %>% 
    addTiles()
  
  # Add layers
  sp_class <- class(sp)[1]
  
  if (grepl("points", sp_class, ignore.case = TRUE)) {
    map <- map %>% 
      addMarkers(popup = popup)
  }
  
  if (grepl("lines", sp_class, ignore.case = TRUE)) {
    map <- map %>% 
      addPolylines(popup = popup)
  }
  
  if (grepl("polygons", sp_class, ignore.case = TRUE)) {
    map <- map %>% 
      addPolygons(popup = popup)
  }
  
  # Return
  map
  
}
