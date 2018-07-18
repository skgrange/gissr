#' Function to export geometries as a GPX file. 
#' 
#' \code{write_gpx} uses \code{rgdal::writeOGR} as the GPX writer. GPX files do 
#' not support polygons, therefore polygon geometries will be coerced to lines.
#' 
#' @param sp Spatial object. 
#' 
#' @param file File name of GPX file.
#' 
#' @param force Should the projection be forced to WGS84? Default is \code{TRUE}
#' because this is the standard for GPX files. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Export a GPX file containing points
#' write_gpx(sp_bus_stations, "~/Desktop/bus_stations.gpx")
#' 
#' # Export GPX file which contains a track
#' write_gpx(sp_gpx_track, "~/Desktop/drive_to_bath.gpx")
#'
#' }
#'
#' @export
write_gpx <- function(sp, file, force = TRUE) {
  
  # Projection force
  if (force) sp <- sp_transform(sp)
  
  # Drop data slot if contains no variables or observations, causes the writer
  # to error
  if (grepl("Data", sp_class(sp)) && (nrow(sp@data) == 0 || ncol(sp@data) == 0))
    sp <- sp_demote(sp)
  
  # Promote
  sp <- sp_promote(sp)
  
  # Get class of object
  sp_class <- sp_class(sp)
  
  # Points
  if (sp_class == "SpatialPointsDataFrame") {
    
    layer <- "points"
    
  } else if (sp_class == "SpatialLinesDataFrame") {
    
    layer <- "tracks"
    
  } else if (sp_class == "SpatialPolygonsDataFrame") {
    
    # Polygons are not supported by gpx files, coerce to lines
    warning(
      "Polygons are not supported by GPX, polygons have been coerced to lines...", 
      call. = FALSE
    )
    
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
  suppressWarnings(
    rgdal::writeOGR(
      sp, 
      dsn = file, 
      layer = layer, 
      driver = "GPX", 
      dataset_options = "GPX_USE_EXTENSIONS=yes"
    )
  )
  
  # No return
  
}
