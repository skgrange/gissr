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
leaflet_plot <- function(sp, popup = NULL, force = TRUE) {
  
  # Sort out popups
  # Use name variable even if not declared
  if (is.null(popup) & "name" %in% names(sp@data)) popup <- "name"
  
  # Parse
  if (!is.null(popup)) popup <- as.formula(stringr::str_c("~ ", popup))
  
  # Projection force
  if (force) sp <- sp_transform(sp, warn = FALSE)
  
  # Create map
  map <- leaflet(sp) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Thunderforest.Landscape", group = "Landscape") %>%
    addProviderTiles("Thunderforest.TransportDark", group = "Transport dark") %>%
    addProviderTiles("Thunderforest.Outdoors", group = "Outdoors") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>% 
    # addProviderTiles(provider = "http://{s}.tile.opentopomap.org/{z}/{x}/{y}.png",
    #                  group = "Topo") %>% 
    addLayersControl(baseGroups = c("OpenStreetMap", "Toner", "Landscape", 
                                    "Transport dark", "Outdoors", "Images"))
  
  # Add layers
  # Find geom type
  sp_class <- class(sp)[1]
  
  if (grepl("points", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addCircleMarkers(popup = popup)
    
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
