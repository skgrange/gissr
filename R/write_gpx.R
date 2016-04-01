#' Function to export geometries as a GPX file. 
#' 
#' \code{write_gpx} uses \code{rgdal::writeOGR} as the GPX writer. GPX files do 
#' not support polygons, therefore polygon geometries will be coerced to lines.
#' 
#' @param sp Spatial object. 
#' 
#' @param file File name of GPX file.
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Export a GPX file containing points
#' write_gpx(sp_bus_stations, "~/Desktop/bus_stations.gpx")
#' 
#' # Export GPX file which contains a track
#' write_gpx(sp_gpx_track, "~/Desktop/drive_to_bath.gpx")
#'
#' }
#'
#' @export
write_gpx <- function(sp, file) {
  
  # Promote
  sp <- sp_promote(sp)
  
  # Get class of object
  sp_class <- class(sp)[1]
  
  # Points
  if (sp_class == "SpatialPointsDataFrame") layer <- "points"

  # Lines
  if (sp_class == "SpatialLinesDataFrame") layer <- "tracks"
  
  # Polygons, not supported by gpx
  if (sp_class == "SpatialPolygonsDataFrame") {
    
    # Polygons are not supported by gpx files, coerce to lines
    warning("Polygons are not supported by GPX, polygons have been coerced to lines.", 
            call. = FALSE)
    
    # Change data type
    sp <- as(sp, "SpatialLinesDataFrame")
    
    # Layer
    layer <- "tracks"
    
  }
  
  # Write GPX file
  # Make sure file name is expanded
  file <- path.expand(file)
  
  # Delete file, rgdal does not do this
  if (file.exists(file)) file.remove(file)
  
  # Export file
  # Warnings when projection is unknown or not wgs86, not really an issue for me
  suppressWarnings(
    rgdal::writeOGR(sp, file, layer = layer, driver = "GPX", 
                    dataset_options = "GPX_USE_EXTENSIONS=yes")
  )
  
}
