#' Function to export geometries as a KML file. 
#' 
#' @param sp Spatial object. 
#' 
#' @param file File name of KML file.
#' 
#' @param force Should the projection be forced to WGS84? Default is \code{TRUE}
#' because this is the standard for KML files. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Export a KML file containing points
#' write_kml(sp_bus_stations, "~/Desktop/bus_stations.kml")
#' 
#' # Export KML file which contains a track
#' write_kml(sp_track, "~/Desktop/drive_to_bath.kml")
#'
#' }
#'
#' @export
write_kml <- function(sp, file, force = TRUE) {
  
  # Projection force
  if (force) sp <- sp_transform(sp)
  
  # Promote
  sp <- sp_promote(sp)
  
  # Get class of object
  sp_class <- sp_class(sp)
  
  # Points
  if (sp_class == "SpatialPointsDataFrame") {
    
    layer <- "points"
    
  } else if (sp_class == "SpatialLinesDataFrame") {
    
    layer <- "lines"
    
  } else if (sp_class == "SpatialPolygonsDataFrame") {
    
    layer <- "polygons"
    
  }
  
  # Write KML file
  # Make sure file name is expanded
  file <- path.expand(file)
  
  # Delete file, rgdal does not do this
  if (file.exists(file)) file.remove(file)
  
  # Export file
  suppressWarnings(
    rgdal::writeOGR(
      sp, 
      dsn = file, 
      layer = layer, 
      driver = "KML"
    )
  )
  
  # No return
  
}
