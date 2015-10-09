#' Function for converting a data frame into a spatial-lines data frame.
#' 
#' \code{data_frame_to_line} returns a SpatialLinesDataFrame with a single line 
#' object. Multiple identifiers are not currently preserved. In general usage, 
#' \code{latitude} and \code{longitude} will be projected in WGS84. 
#' \code{latitude} and \code{longitude} must not contain \code{NA} values 
#' because this is a limitation of the spatial object. 
#' 
#' @param df Data frame to be converted into SpatialLinesDataFrame. 
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' sp_lines <- data_frame_to_line(data_gps_track, "latitude", "longitude")
#' }
#' 
#' @export
#' 
data_frame_to_line <- function (df, latitude = "latitude", 
                                longitude = "longitude") {
  
  # Make an identifier variable for lines
  if (!"id" %in% names(df)) {
    df[, "id"] <- 1
  }
  
  # If ID does not start with 1, the id's do not match later
  # Happens post-filtering
#   df[, "id"] <- as.numeric(as.character(df[, "id"]))
#   id.min <- min(df[, "id"])
#   df[, "id"] <- df[, "id"] - id.min
  
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
  
  # Force projection
  sp::proj4string(lines) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  # Make SpatialLinesDataFrame
  lines <- sp::SpatialLinesDataFrame(lines, data_extras)
  
  # Return
  lines
  
}
