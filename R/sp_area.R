#' Function to calculate areas of spatial polygons in metres-squared, 
#' kilometres-squared, or hectares. 
#' 
#' If the spatial-polygons are projected in a system with a metre unit,
#' \code{rgeos::gArea} is used to calculate the area of the polygons. If the
#' projection system is WGS84, a trigonometric function is used 
#' (\code{geosphere::areaPolygon}). Although using a projection system with 
#' metres yields the most accurate results, the \code{geosphere::areaPolygon} 
#' function has been tested and performs well. 
#' 
#' @param sp Spatial-polygons object.
#' 
#' @param unit Unit of returned area. Can be "m", "km", or "ha". Default is "m".
#' 
#' @param features Should the individual areas of the features contained within
#' \code{sp} be calculated? Default is \code{TRUE}.
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Calculate the area of the Greater London Built-up area, sp is projected 
#' in OSGB36, a metric projection system
#'
#' sp_area(sp_built_up_london, unit = "km")
#' 1737.855
#'
#' # Wikipedia says the same
#' # https://en.wikipedia.org/wiki/Greater_London_Built-up_Area
#' }
#' 
#' @export
sp_area <- function(sp, unit = "m", features = TRUE) {
  
  # Unit check
  if (!unit %in% c("m", "km", "ha", "ac"))
    stop("Unit must be 'm', 'km', 'ha', or 'ac'.", call. = FALSE)
  
  # Use a trigonometric function for latitude and longitude pairs
  if (sp_projection(sp) %in% 
      c("+proj=longlat +datum=WGS84 +no_defs", 
        "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", 
        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) {
    
    # Calculate area
    vector <- geosphere::areaPolygon(sp, byid = features)
    
  } 
  
  # Use rgeos when the projection unit is in metres
  if (grepl("+units=m", proj4string(sp))) {
    
    # Spits a warning about coordinates, can be ignored
    suppressWarnings(
      vector <- rgeos::gArea(sp, byid = features)
    )
    
    # Drop names
    vector <- unname(vector)
    
  }
  
  # Transform units
  if (unit == "km") vector <- vector / 1000000
  if (unit == "ha") vector <- vector / 10000
  if (unit == "ac") vector <- vector * 0.000247105381
  
  # Return
  vector
  
}
