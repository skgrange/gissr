#' Function for point-in-polygon tests. 
#' 
#' \code{sp_left_join} tests if a point is within a polygon and joins data from
#' a spatial object (the data slot of the spatial-polygon data frame) if the 
#' match is \code{TRUE}. This process is analogous to a SQL left join but with
#' spatial objects. If variable names collide, they will be suffixed with 
#' \code{"_polygons"}. 
#' 
#' \code{over} is used for the point-in-polygon test and the projection systems
#' must be identical for the two geometries. 
#' 
#' @param sp_points Spatial points object. Spatial points will usually have a 
#' data slot but \code{sp_left_join} does not require this. 
#' 
#' @param sp_polygons Spatial polygons object. \code{sp_polygons} will have a 
#' data slot (\emph{i.e.} it will be a \code{SpatialPolygonsDataFrame}) because
#' this is what is joined.
#'
#' @seealso \code{\link{sp_transform}}, \code{\link{over}}, \code{\link{merge}},
#' \code{\link{sp_from_data_frame}}
#' 
#' @author Stuart K. Grange
#' 
#' @return A tibble, not a spatial object. 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # I am in London, I know my latitude and longitude. Which borough am I in? 
#' 
#' # Load geojson containing the London boroughs
#' sp_london <- sp_read(
#'   "http://skgrange.github.io/www/data/london_sport.json",
#'   verbose = FALSE
#' ) %>% 
#'   sp_transform()
#' 
#' # Make latitude and longitude a data frame, observation is optional
#' data_points <- data.frame(
#'   observation = "current_location",
#'   latitude = 51.523595, 
#'   longitude = -0.027114
#' )
#' 
#' # Promote to spatial points, this will use wgs84 when not stated
#' sp_points <- sp_from_data_frame(data_points, type = "points")
#' 
#' # Test point with 33 polygons (boroughs) in the shape file
#' data_point_test <- sp_left_join(sp_points, sp_london)
#' 
#' # Print
#' data_point_test$name
#' 
#' "Tower Hamlets"
#' 
#' }
#' 
#' @export
sp_left_join <- function(sp_points, sp_polygons) {
  
  # Check the spatial objects 
  if (!grepl("points", sp_class(sp_points), ignore.case = TRUE)) {
    stop(
      "Spatial points must be defined in the `sp_points` argument.", 
      call. = FALSE
    )
  }
  
  if (!grepl("polygon", sp_class(sp_polygons), ignore.case = TRUE)) {
    stop(
      "Spatial-polygons must be defined in the `sp_polygons` argument.", 
      call. = FALSE
    )
  }
  
  # Store data slot of points
  if (grepl("data", sp_class(sp_points), ignore.case = TRUE)) {
    df <- sp_points@data
  }
  
  # The point in polygon function, returns data frame
  df_sp <- over(sp_points, sp_polygons, fn = NULL)
  
  if (grepl("data", sp_class(sp_points), ignore.case = TRUE))  {
    
    # Bind the variables
    df <- dplyr::bind_cols(df, df_sp)
    
    # Check for unique names
    if (any(duplicated(names(df)))) {
      # Unique names
      names(df) <- make.names(names(df), unique = TRUE)
      names(df) <- stringr::str_replace(names(df), ".1$", "_polygons")
    } 
  
  } else {
    # Reassign over return
    df <- df_sp
  }
  
  # To tibble
  df <- as_tibble(df)
  
  return(df)
  
}
