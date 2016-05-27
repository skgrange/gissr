#' Function to convert coordinates within a data frame. 
#' 
#' \code{transform_coordinates} converts coordinate pairs from one projection 
#' system to another. \code{transform_coordinates} is useful when tabular data 
#' are supplied and the coordinate system is different that what is desired. 
#' 
#' \code{transform_coordinates} works by coercing the input data frame to a 
#' spatial object, applies \code{sp::spTransform} to convert the coordinates, 
#' converts the spatial object back to a data frame and then returns the data 
#' frame with the transformed coordinates. The transformed coordinates can be 
#' optionally renamed and reordered. 
#' 
#' \code{transform_coordinates} requires a CRS projection string for the 
#' \code{from} and \code{to} arguments. \code{to} by default is set as 
#' WGS 84/EPSG:4326 (\code{+proj=longlat +datum=WGS84}).
#' 
#' @param df Data frame with coordinates to be transformed. 
#' 
#' @param latitude Name of latitude/y variable. 
#' 
#' @param longitude Name of longitude/x variable. 
#' 
#' @param from A proj4 string which represents what coordinate system the
#' data frame is within. 
#' 
#' @param to A proj4 string which represents what coordinate system the 
#' converted coordinates will be converted to.
#' 
#' @seealso \code{\link{sp_transform}}, \code{\link{sp_from_data_frame}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Convert British National Grid/Ordnance Survey National Grid/OSGB36/EPSG:7405
#' # to latitude and longitude (WGS 84/EPSG:4326)
#' 
#' # The proj4 string for British National Grid (very long string!)
#' bng <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs"
#' 
#' data_oxford_transform <- transform_coordinates(
#'   data_oxford, latitude = "latitude", longitude = "longitude", from = bng)
#'   
#' }
#'
#' @export
transform_coordinates <- function(df, latitude = "latitude", longitude = "longitude", 
                                  from, to) {
  
  # Get variable order
  variables <- names(df)
  
  # Promote
  sp <- sp_from_data_frame(df, latitude, longitude, projection = from, 
                           type = "points")
  
  # Do the projection conversion
  sp <- sp_transform(sp, to)
  
  # Back to data frame
  df <- data.frame(sp)
  
  # Remove optional variable
  df$optional <- NULL
  
  df <- threadr::arrange_left(df, variables)
  
  # Return
  df
  
}
