#' Function for converting a data frame into a spatial-points data frame.
#' 
#' \code{data_frame_to_points} conveniently transforms a data frame to a 
#' SpatialPointsDataFrame. 
#' 
#' In general usage, \code{latitude} and \code{longitude} will be projected in 
#' WGS84. \code{latitude} and \code{longitude} must not contain \code{NA} values 
#' because this is a limitation of the spatial object. 
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
#' sp_points <- data_frame_to_line(data_monitoring_sites, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_points <- function (df, latitude = "latitude", 
                                  longitude = "longitude", 
                                  projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp_object <- df
  
  # Give the object a projection
  sp_object <- sp_transform(sp_object, projection, warn = FALSE)
  
  # Return
  sp_object
  
}



#' Function for converting a data frame into a spatial-lines data frame.
#' 
#' \code{data_frame_to_line} returns a SpatialLinesDataFrame with a single line 
#' object. Multiple identifiers are not currently preserved. 
#' 
#' In general usage, \code{latitude} and \code{longitude} will be projected in 
#' WGS84. \code{latitude} and \code{longitude} must not contain \code{NA} values 
#' because this is a limitation of the spatial object. 
#' 
#' @param df Data frame to be converted into SpatialLinesDataFrame. 
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param force Force all lines to have the same id? This is needed to keep data
#' and lines within the same unit and therefore allow for correct binding. 
#' Default is \code{TRUE}.
#' 
#' @aliases data_frame_to_polygons
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' sp_lines <- data_frame_to_line(data_gps_track, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_line <- function (df, latitude = "latitude", 
                                longitude = "longitude", 
                                projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                                force = TRUE) {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make an identifier variable for lines
  if (force) {
    df[, "id"] <- 1
  }
  
  # Get data part for the SpatialLinesDataFrame
  data_extras <- data.frame(id = unique(df[, "id"]))
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp_object <- df
  
  # From
  # http://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-
  # to-spatiallinesdataframe-in-r
  # Generate lines for each id
  lines <- lapply(split(sp_object, sp_object$id), 
                  function(x) sp::Lines(list(sp::Line(sp::coordinates(x))), x$id[1L]))
  
  # Create SpatialLines
  lines <- sp::SpatialLines(lines)
  
  # Give projection
  lines <- sp_transform(lines, projection, warn = FALSE)
  
  # Make SpatialLinesDataFrame
  lines <- sp::SpatialLinesDataFrame(lines, data_extras)
  
  # Return
  lines
  
}



#' Function for converting a data frame to spatial-polygon data frame.
#' 
#' \code{data_frame_to_polygon} conveniently transforms a data frame to a 
#' SpatialPolygonsDataFrame. 
#' 
#' \code{data_frame_to_polygon} will create closed polygons by joining the first
#' and last observations together in a straight line if the input data frame's 
#' first and last coordinate pairs to not match. \code{data_frame_to_polygon} 
#' correctly deals with holes if the variable \code{"hole"} is present in the 
#' input data frame. 
#' 
#' In general usage, \code{latitude} and \code{longitude} will be projected in 
#' WGS84. \code{latitude} and \code{longitude} must not contain \code{NA} values 
#' because this is a limitation of the spatial object. 
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
#' @param force Force all polygons to have the same group? This is needed to keep 
#' data and polygons within the same unit and therefore allow for correct binding. 
#' Default is \code{TRUE}.
#' 
#' @aliases data_frame_to_polygons
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Convert a hand-drawn line from an online application to a polygon
#' sp_polygon <- data_frame_to_polygon(data_drawn, "latitude", "longitude")
#' }
#' 
#' @export
data_frame_to_polygon <- function (df, latitude = "latitude", 
                                   longitude = "longitude", 
                                   projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                                   force = TRUE) {
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Add group variable if it does not exist
  if (force) {
    df[, "group"] <- 1
  }
  
  # Reset row names
  row.names(df) <- NULL
  
  # Get extras from input data frame
  other_index <- which(names(df) %ni% c(latitude, longitude))
  # Only first row. Ok? 
  data_extras <- df[other_index][1, ]
  
  # A catch for when only group is present, i.e. there are no extra identifiers
  # in df. To-do: do this better. 
  if (class(data_extras) == "numeric") {
    data_extras <- data.frame(group = data_extras)
  }
  
  # Get coordinate list
  # A list element will represent each group within a feature 
  # Long-lat order is important
  coordinates <- plyr::dlply(df, "group", function(x) 
    data.matrix(x[, c(longitude, latitude)]))
  
  # List of individual polygons
  polygons <- lapply(coordinates, sp::Polygon)
  # Polygons as one object
  polygons <- sp::Polygons(polygons, 1)
  
  # Make sp class
  polygons_sp <- sp::SpatialPolygons(list(polygons))
  
  # Give projection
  polygons_sp <- sp_transform(polygons_sp, projection, warn = FALSE)
  
  # Make sp dataframe
  polygons_sp <- sp::SpatialPolygonsDataFrame(polygons_sp, data_extras)
  
  # Return
  polygons_sp
  
}

# Define the negative %in% function
`%ni%` <- Negate(`%in%`)

