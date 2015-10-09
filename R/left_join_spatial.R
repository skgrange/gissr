#' Function for a point-in-polygon test. 
#' 
#' \code{left_join_spatial} tests if a point is within a polygon and joins 
#' data from a spatial object (the data component of the spatial-polygon data
#' frame) if the match is TRUE. This process is analogous to a SQL left join 
#' with the match being a spatial intersection. 
#' 
#' Points to be tested are generally stored in a data frame with latitude and 
#' longitude pairs, while the polygons are stored in a spatial-polygon object. 
#' The result is the input data frame with the joined data contained within the 
#' spatial-polygon data frame. Observations which do not match are filled with
#' \code{NA}.  
#' 
#' \code{sp::over} is used for the point-in-polygon test. 
#' 
#' @param df Data frame containing latitude and longitude variables. 
#' @param latitude \code{df}'s latitude variable name.
#' @param longitude \code{df}'s longitude variable name.
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' @param polygons A spatial-polygon data frame to be joined to \code{df}.
#' 
#' @seealso See \code{\link{spTransform}}, \code{\link{over}}, 
#' \code{\link{merge}}, \code{\link{join}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Join air quality zones to latitude and longitude pairs
#' data_join <- left_join_spatial(data_coordinates, latitude = "latitude", 
#'   longitude = "longitude", polygons = shape_file_pm10)
#' 
#' 
#' # I am in London, I know my latitude and longitude. Which borough am I in? 
#' 
#' # Load shape file containing the London boroughs
#' shape_file <- sp_read("data_objects/london_boroughs/london_sport.shp")
#' 
#' # Make latitude and longitude a data frame
#' data_point <- data.frame(latitude = 51.523595, longitude = -0.027114)
#' 
#' # Test point with 33 polygons (boroughs) in the shape file
#' left_join_spatial(data_point, polygons = shape_file)
#' "Tower Hamlets"
#' }
#' 
#' @export
#' 
left_join_spatial <- function (df, latitude = "latitude", 
                               longitude = "longitude", 
                               projection = "+proj=longlat +datum=WGS84", 
                               polygons = NA) {
  
  # Check the spatial object
  if (!grepl("polygon", class(polygons), ignore.case = TRUE)) {
    stop("Spatial-polygons must be defined in the 'polygon' argument. ")
  }
  
  # Catch for dplyr's data frame class
  if ("tbl" %in% class(df)) {
    df <- data.frame(df)
  }
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp_object <- df
  
  # Give the object a projection
  proj4string(sp_object) <- projection
  
  # The point in polygon function
  df_match <- sp::over(sp_object, polygons, fn = NULL)
  
  # Input back to data frame
  df <- data.frame(sp_object)
  
  # Drop logical optional variable which occurs during projection manipulations
  if ("optional" %in% names(df)) {
    df[, "optional"] <- NULL
  }
  
  # Add joined variable to data frame
  df <- cbind(df, df_match)
  
  # Return
  df
  
}
