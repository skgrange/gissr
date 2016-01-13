#' Function to write a data frame to a GPX file with usage analogous to 
#' \code{write.table}.
#' 
#' \code{write_gpx} uses \code{rgdal::writeOGR} as the GPX writer. Unlike the 
#' standard \code{writeOGR} function, \code{write_gpx} will automatically expand 
#' file paths and can overwrite previous files if necessary. 
#'
#' \code{latitude} and \code{longitude} must not contain \code{NA} values.
#' 
#' To-do: Does not work with spatial objects without data. Make sure it does. I
#' think this is an \code{writeOGR} issue so will need to upgrade the spatial 
#' ojects in a robust way. 
#' 
#' @param df Data frame to be written to a GPX file.
#' 
#' @param file File name of GPX file.
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param name Name of variable which will be added in the \code{"name"} element
#' of the GPX file. Optional. 
#' 
#' @param layer Type of layer to be written to GPX file. Can either be 
#' 
#' \code{"points"} or \code{"lines"}. Default is \code{"points"}. 
#'   
#' @author Stuart K. Grange
#' 
#' @examples
#' 
#' \dontrun{
#' # Export a GPX file containing points
#' write_gpx(data_bus_stations, "~/Desktop/bus_stations.gpx")
#' 
#' # Export GPX file which contains a line object
#' write_gpx(data_gpx_track, "~/Desktop/drive_to_bath.gpx", layer = "lines")
#'
#' }
#'
#' @export
write_gpx <- function (df, file, latitude = "latitude", longitude = "longitude", 
                       name = NA, layer = "points") {
  
  # For data frames
  if (grepl("data.frame", class(df), ignore.case = TRUE)) {
    
    # Check
    if (!layer %in% c("points", "lines")) {
      stop("Layer must either be 'points' or 'lines'.")
    }
    
    # Add a name element
    if (!is.na(name)) {
      df[, "name"] <- df[, name]
    }
    
    # Make spatial points
    if (layer == "points") {
      
      # A catch for when only coordinates are present, i.e. there are no extra 
      # identifiers in df. To-do: do this better. 
      if (ncol(df) == 2) {
        df[, "name"] <- ""
      }
      
      # Make sp points object
      sp::coordinates(df) <- c(longitude, latitude)
      
      # Reassign
      sp_object <- df
      
      # Force projection, not ideal
      sp::proj4string(sp_object) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      
      # For writeOGR
      layer_vector <- "points"
      
    }
    
    # Make spatial lines
    if (layer == "lines") {
      
      # Use function
      sp_object <- data_frame_to_line(df, latitude, longitude)
      
      # For writeOGR
      layer_vector <- "lines"
      
    }
    
  }
  
  # For spatial objects
  # Points
  if (grepl("spatialpoints", class(df), ignore.case = TRUE)) {
    
    # Re-assign input
    sp_object <- df
    
    if (!is.na(name)) {
      
      # Extract vector for gpx name
      name_vector <- sp_object@data[, name]
      
      # Add vector to object
      sp_object@data[, "name"] <- name_vector
      
    }
    
    # For writeOGR
    layer_vector <- "points"
    
  }
  
  # Lines
  if (grepl("spatiallines", class(df), ignore.case = TRUE)) {
    
    # Simply re-assign input
    sp_object <- df
    
    if (!is.na(name)) {
      
      # Extract vector for gpx name
      name_vector <- sp_object@data[, name]
      
      # Add vector to object
      sp_object@data[, "name"] <- name_vector
      
    }
    
    # For writeOGR
    layer_vector <- "tracks"
    
  }
  
  # Polygons
  if (grepl("spatialpolygons", class(df), ignore.case = TRUE)) {
    
    # Polygons are not supported by gpx files, convert to lines
    message("Polygons are not supported by GPX, the polygons have been coerced to lines.")
    
    # Add a data slot if polygons does not contain one
    if (!grepl("data", class(df), ignore.case = TRUE)) {
      df <- SpatialPolygonsDataFrame(df, 
                                     data.frame(id = as.character(1:length(df))))
      
    }
    
    # To lines
    sp_object <- as(df, "SpatialLinesDataFrame")

    if (!is.na(name)) {
      
      # Extract vector for gpx name
      name_vector <- sp_object@data[, name]
      
      # Add vector to object
      sp_object@data[, "name"] <- name_vector
      
    }
    
    # For writeOGR
    layer_vector <- "tracks"
    
  }
  
  # Write GPX file
  # Make sure file name is expanded
  file <- path.expand(file)
  
  # Delete file, rgdal does not do this
  if (file.exists(file)) {
    file.remove(file)
  }
  
  # Export file
  # Warnings when projection is unkown, not really an issue for me
  suppressWarnings(
    rgdal::writeOGR(sp_object, file, layer = layer_vector, driver = "GPX", 
                    dataset_options = "GPX_USE_EXTENSIONS=yes")
  )
  
}
