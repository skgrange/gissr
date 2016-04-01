#' Function for converting a data frame to a SpatialPointsDataFrame. 
#' 
#' @param df Data frame to be converted into SpatialPointsDataFrame. 
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' sp_points <- data_frame_to_points(data_monitoring_sites, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_points <- function(df, latitude = "latitude", 
                                 longitude = "longitude", 
                                 projection = "+proj=longlat +datum=WGS84 +no_defs") {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp <- df
  
  # Give the object a projection
  sp <- sp_transform(sp, projection, warn = FALSE)
  
  # Return
  sp
  
}



#' Function for converting a data frame to a SpatialLinesDataFrame.
#' 
#' @param df Data frame to be converted into a SpatialLinesDataFrame.
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param id What variable in \code{df} should be used to create separate line 
# features? If \code{id} is not used, a single feature will be created. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' sp_lines <- data_frame_to_lines(data_gps_track, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_lines <- function(df, latitude = "latitude", 
                                longitude = "longitude", 
                                projection = "+proj=longlat +datum=WGS84 +no_defs", 
                                id = NA) {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make an identifier variable for lines
  if (is.na(id)) {
    
    # Single line object, no grouping
    df[, "id"] <- 1
    
  } else {
    
    # Use input variable
    df[, "id"] <- df[, id]
    
  }
  
  # Get data part for the SpatialLinesDataFrame
  data_extras <- dplyr::distinct(df, id)
  
  # Make sp points object
  sp_object <- data_frame_to_points(df, latitude, longitude, projection)
  
  # From
  # http://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-
  # to-spatiallinesdataframe-in-r
  # Generate lines for each id
  lines <- lapply(split(sp_object, sp_object$id), function(x) 
    Lines(list(Line(sp::coordinates(x))), x$id[1L]))
  
  # Drop
  if (!is.na(id)) data_extras[, "id"] <- NULL
  
  # Create SpatialLines
  sp <- sp::SpatialLines(lines)
  
  # Make SpatialLinesDataFrame
  sp <- sp::SpatialLinesDataFrame(sp, data_extras, match.ID = FALSE)
  
  # Give projection
  sp <- sp_transform(sp, projection, warn = FALSE)
  
  # Return
  sp
  
}


#' Function for converting a data frame to SpatialPolygonsDataFrame.
#' 
#' \code{data_frame_to_polygon} will create closed polygons by joining the first
#' and last observations together in a straight line if the input data frame's 
#' first and last coordinate pairs to not match.
#' 
#' @param df Data frame to be converted into SpatialPolygonsDataFrame. 
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param What variable in \code{df} should be used to create separate line 
# features? If \code{id} is not used, a single feature will be created. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Convert a hand-drawn line from an online application to a polygon
#' sp_polygon <- data_frame_to_polygons(data_drawn, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_polygons <- function(df, latitude = "latitude", 
                                   longitude = "longitude", 
                                   projection = "+proj=longlat +datum=WGS84 +no_defs",
                                   id = NA) {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make an identifier variable for lines
  if (is.na(id)) {
    
    # Single line object, no grouping
    df[, "id"] <- 1
    
  } else {
    
    # Use input variable
    df[, "id"] <- df[, id]
    
  }
  
  # Get data part for the SpatialLinesDataFrame
  data_extras <- dplyr::distinct(df, id)
  
  # Make sp points object
  sp <- data_frame_to_points(df, latitude, longitude, projection)
  
  # A list element will represent each group within a feature 
  # Long-lat order is important
  coordinates <- plyr::dlply(df, "id", function (x) 
    data.matrix(x[, c(longitude, latitude)]))
  
  # Make polygons
  sp <- lapply(seq_along(coordinates), function (x) 
    matrix_to_sp_polygon(coordinates[x], x))
  
  # Bind geometries
  sp <- do.call(rbind, sp)
  
  # Make sp dataframe
  sp <- sp::SpatialPolygonsDataFrame(sp, data_extras)
  
  # Give projection
  sp <- sp_transform(sp, projection, warn = FALSE)
  
  # Return
  sp
  
}


matrix_to_sp_polygon <- function (matrix, id) {
  
  # Matix to polygon
  polygon <- Polygon(matrix)
  
  # Polygon to polygons
  polygon <- Polygons(list(polygon), id)
  
  # To spatial polygons
  sp <- sp::SpatialPolygons(list(polygon))
  
  # Return
  sp
  
}


# Deprecate some functions
#' @export
data_frame_to_line <- function(df, latitude = "latitude", 
                               longitude = "longitude", 
                               projection = "+proj=longlat +datum=WGS84 +no_defs", 
                               id = NA) {
  
  # Message
  .Deprecated("data_frame_to_lines", package = "gissr")
  
  # Use function
  sp <- data_frame_to_lines(df, latitude, longitude, projection, id)
  
  # Return
  sp
}


#' @export
data_frame_to_polygon <- function(df, latitude = "latitude", 
                                  longitude = "longitude", 
                                  projection = "+proj=longlat +datum=WGS84 +no_defs",
                                  id = NA) {
  
  # Message
  .Deprecated("data_frame_to_polygons", package = "gissr")
  
  # Use function
  sp <- data_frame_to_polygons(df, latitude, longitude, projection, id)
  
  # Return
  sp
}
