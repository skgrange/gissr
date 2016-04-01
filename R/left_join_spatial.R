#' Function for point-in-polygon tests. 
#' 
#' \code{left_join_spatial} tests if a point is within a polygon and joins 
#' data from a spatial object (the data slot of the spatial-polygon data frame)
#' if the match is \code{TRUE}. This process is analogous to a SQL left join but
#' with a spatial object.
#' 
#' Points to be tested are generally stored in a data frame with latitude and 
#' longitude pairs (or any other projection system), while the polygons are 
#' stored in a spatial-polygon object. The result is the input data frame, with
#' the joined data contained within the spatial-polygon data frame. Observations
#' which do not match are filled with \code{NA}. 
#' 
#' \code{sp::over} is used for the point-in-polygon test and the projection which
#' is used for the data frame must be identical to that within the spatial object. 
#' 
#' @param df Data frame containing latitude and longitude (or other coordinate
#' pair) variables. 
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param polygons A spatial-polygon data frame to be joined to \code{df}.
#' 
#' @seealso \code{\link{sp_transform}}, \code{\link{over}}, \code{\link{merge}}
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
left_join_spatial <- function(df, latitude = "latitude", 
                              longitude = "longitude", 
                              projection = "+proj=longlat +datum=WGS84 +no_defs", 
                              polygons = NA) {
  
  # Check the spatial object
  if (!grepl("polygon", class(polygons), ignore.case = TRUE))
    stop("Spatial-polygons must be defined in the 'polygons' argument.", 
         call. = FALSE)
  
  # Catch for dplyr's data frame class
  df <- threadr::base_df(df)
  
  # Make sp points object
  sp::coordinates(df) <- c(longitude, latitude)
  
  # Reassign
  sp_object <- df
  
  # Give the object a projection
  sp_object <- sp_transform(sp_object, projection, warn = FALSE)
  
  # The point in polygon function
  df_match <- sp::over(sp_object, polygons, fn = NULL)
  
  # Input back to data frame
  df <- data.frame(sp_object)
  
  # Drop logical optional variable which occurs during projection manipulations
  if ("optional" %in% names(df)) df <- df[, -grep("optional", names(df))]
  
  # Add joined variable to data frame
  df <- cbind(df, df_match)
  
  # Return
  df
  
}


#' Function for extracting values from a raster object based on points contained
#' within a data frame.
#' 
#' \code{left_join_raster} tests if a point is within a raster object and returns
#' the raster's object value if \code{TRUE}. This process is analogous to a SQL 
#' left join but with a raster object. In raster applications, this is also 
#' called an \emph{extraction} or \emph{location} process.
#' 
#' Points to be tested are generally stored in a data frame with latitude and 
#' longitude pairs (or any other projection system) and the raster object must 
#' have the same projection system. The result is the input data frame, with
#' the joined raster values. Observations which do not match are filled with 
#' \code{NA}. 
#' 
#' Currently only one layer is used with no buffer. No interpolation or
#' aggregation functions are used either. 
#' 
#' \code{raster::extract} is used for the join. 
#' 
#' @param df Data frame containing latitude and longitude (or other coordinate
#' pair) variables. 
#' 
#' @param latitude \code{df}'s latitude variable name.
#' 
#' @param longitude \code{df}'s longitude variable name.
#' 
#' @param projection \code{df}'s latitude and longitude projection system. 
#' Default is WGS84.
#' 
#' @param raster A raster object to be joined to \code{df}.
#' 
#' @param name The name of \code{df}'s new variable sourced from \code{raster}. 
#' Default is \code{"raster_value"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Get ozone concentrations from a raster object
#' data_select <- left_join_raster(data_select, raster = raster_ozone, 
#'                                 name = "ozone")
#' 
#' }
#' 
#' @export
left_join_raster <- function(df, latitude = "latitude", 
                             longitude = "longitude", 
                             projection = "+proj=longlat +datum=WGS84 +no_defs",
                             raster, name = "raster_value") {
  
  # Promote to spatial object
  sp <- data_frame_to_points(df, latitude = latitude, longitude = longitude,
                             projection = projection)
  
  # Get values for points
  vector <- raster::extract(raster, sp, method = "simple")
  
  # Add vector to data frame
  df[, name] <- vector
  
  # Return
  df
  
}
