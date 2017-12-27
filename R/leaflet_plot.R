#' Convenience function to quickly plot geometries on a leaflet map. 
#' 
#' @param sp Spatial object to be plotted. 
#'
#' @param popup Vector of variables to be used as a popup on the map. 
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
#' @return Invisible, a leaflet map. 
#' 
#' @import leaflet
#' 
#' @export
leaflet_plot <- function(sp, popup = NULL, force = TRUE, colour = "#03F", 
                         opacity = 0.5, fill_opacity = 0.2) {
  
  # Find geom type
  sp_class <- sp_class(sp)
  
  # Sort out popups
  # Use name variable even if not declared
  if (grepl("data", sp_class, ignore.case = TRUE)) {
    
    # Use a default
    if (is.null(popup) && "name" %in% names(sp@data)) popup <- "name"
    
    if (!is.null(popup)) {
      
      # Select variables in data slot, no comma to keep data frame structure
      df_sp <- sp@data[popup]
      
      # Catch nas, make a string
      df_sp[is.na(df_sp)] <- ""
      
      # Get variable names
      names <- names(df_sp)
      
      # Give names column-wise
      popup_string <- apply(df_sp, 1, collapse_values_with_name, name = names)
      
      # Collapse row-wise, now just a vector
      if (class(popup_string) == "matrix")
        popup_string <- apply(popup_string, 2, stringr::str_c, collapse = "<br>")
      
      # No names for the vector
      popup_string <- unname(popup_string)
      
      # Reassign
      popup <- popup_string
      
    }
    
  }
  
  # 
  # Projection force
  if (force) sp <- sp_transform(sp, warn = FALSE)
  
  # Create map
  map <- leaflet(sp) %>%
    addTiles(
      group = "OpenStreetMap", 
      urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner lite") %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "landscape", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Landscape") %>% 
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "transport-dark", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Transport dark") %>% 
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "outdoors", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Outdoors") %>% 
    addProviderTiles("Esri.WorldImagery", group = "Images") %>% 
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Toner", "Toner lite", "Landscape", 
                     "Transport dark", "Outdoors", "Images"))
  
  # Add layers
  if (grepl("points", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addCircleMarkers(
        popup = popup, 
        color = colour, 
        opacity = opacity,
        fillOpacity = fill_opacity
      )
    
  } else if (grepl("lines", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addPolylines(
        popup = popup, 
        color = colour, 
        opacity = opacity,
        fillOpacity = fill_opacity
      )
    
  } else if (grepl("polygons", sp_class, ignore.case = TRUE)) {
    
    map <- map %>% 
      addPolygons(
        popup = popup, 
        color = colour, 
        opacity = opacity,
        fillOpacity = fill_opacity
      )
    
  } else if (sp_class == "RasterLayer") {
    
    # Just a first step, raster and layer control has some issues
    map <- leaflet() %>% 
      addTiles(
        group = "OpenStreetMap", 
        urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      ) %>% 
      addRasterImage(sp, colors = "viridis", opacity = opacity)
    
  }
  
  return(map)
  
}


collapse_values_with_name <- function(x, name, sep = ": ") 
  stringr::str_c(name, sep, x)
