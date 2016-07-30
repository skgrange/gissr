#' Function for point-in-polygon tests. 
#' 
#' \code{sp_left_join} tests if a point is within a polygon and joins data from
#' a spatial object (the data slot of the spatial-polygon data frame) if the 
#' match is \code{TRUE}. This process is analogous to a SQL left join but with
#' spatial objects.
#' 
#' \code{sp::over} is used for the point-in-polygon test and the projection 
#' systems must be identical for the two geometries. 
#' 
#' @param sp_points Spatial points object. 
#' 
#' @param sp_polygons Spatial polygons object. 
#'
#' @seealso \code{\link{sp_transform}}, \code{\link{over}}, \code{\link{merge}},
#' \code{\link{sp_from_data_frame}}
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # I am in London, I know my latitude and longitude. Which borough am I in? 
#' 
#' # Load shape file containing the London boroughs
#' sp_london <- sp_read("data_objects/london_boroughs/london.shp")
#' 
#' # Make latitude and longitude a data frame
#' data_points <- data.frame(latitude = 51.523595, longitude = -0.027114)
#' 
#' # Promote to spatial points, this will use wgs84 when not stated
#' sp_points <- sp_from_data_frame(data_points, type = "points")
#' 
#' # Test point with 33 polygons (boroughs) in the shape file
#' sp_left_join(sp_points, sp_london)
#' "Tower Hamlets"
#' }
#' 
#' @export
sp_left_join <- function(sp_points, sp_polygons) {
  
  # Check the spatial object
  if (!grepl("polygon", class(sp_polygons), ignore.case = TRUE))
    stop("Spatial-polygons must be defined in the 'polygons' argument.", 
         call. = FALSE)
  
  # The point in polygon function
  df <- sp::over(sp_points, sp_polygons, fn = NULL)
  
  # Return
  df
  
}
