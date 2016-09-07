#' Convenience function to quickly plot geometries on a leaflet map. 
#' 
#' @param sp Spatial object to be plotted. 
#'
#' @param popup Popup variable to add to map. 
#'
#' @param force Should the projection be forced to WGS84? Default is \code{TRUE}.
#' 
#' @param colour Colour of geometry. 
#' 
#' @param opacity Opacity of the edge of the geometry.
#' 
#' @param fill_opacity Internal opacity of the geometry.
#' 
#' @author Stuart K. Grange
#' 
#' @import leaflet
#' 
#' @export
leaflet_plot <- function(sp, popup = NULL, force = TRUE, colour = "#03F", 
                         color = colour, opacity = 0.5, fill_opacity = 0.2) {
  
  # Find geom type
  sp_class <- sp_class(sp)
  
  # Sort out popups
  # Use name variable even if not declared
  if (grepl("data", sp_class, ignore.case = TRUE))
    if (is.null(popup) & "name" %in% names(sp@data)) popup <- "name"
  
  # Parse
  if (!is.null(popup)) popup <- as.formula(stringr::str_c("~ ", popup))
  
  # Projection force
  if (force) sp <- sp_transform(sp, warn = FALSE)
  
  # Create map
  map <- leaflet(sp) %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner lite") %>%
    # addProviderTiles("Stamen.Terrain", group = "Terrain") %>%
    addProviderTiles("Thunderforest.Landscape", group = "Landscape") %>%
    addProviderTiles("Thunderforest.TransportDark", group = "Transport dark") %>%
    addProviderTiles("Thunderforest.Outdoors", group = "Outdoors") %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>% 
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Toner", "Toner lite", "Landscape", 
                     "Transport dark", "Outdoors", "Images"))
  
  # Add layers
  if (grepl("points", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addCircleMarkers(popup = popup, color = colour, opacity = opacity,
                       fillOpacity = fill_opacity)
    
  }
  
  if (grepl("lines", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addPolylines(popup = popup, color = colour, opacity = opacity,
                   fillOpacity = fill_opacity)
    
  }
  
  if (grepl("polygons", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addPolygons(popup = popup, color = colour, opacity = opacity,
                  fillOpacity = fill_opacity)
    
  }
  
  # Return
  map
  
}
