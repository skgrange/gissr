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
