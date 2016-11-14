#' Function to find elevation from latitude and longitude coordinate pairs using
#' Google Maps API. 
#' 
#' The Google Maps API is not open-source, therefore check the licensing 
#' conditions for usage conditions 
#' (\url{https://developers.google.com/maps/terms}). Users of this function 
#' must use the elevation data to display on a Google Map. 
#'
#' @return Either a data frame containing latitude and longitude
#' pairs, elevation estimate, and (horizontal) resolution of elevation estimate 
#' or a numeric vector containing only elevation estimates. 
#' 
#' @import dplyr
#' @importFrom jsonlite fromJSON
#' 
#' @param latitude Numeric vector of latitude values.
#' 
#' @param longitude Numeric vector of longitude values.
#' 
#' @param key Google Maps API key. 
#' 
#' @param round Number of decimal points to round the elevation variable to.
#' Default is no rounding. 
#' 
#' @param vector Should the function only return a vector of elevation? Default
#' is \code{TRUE} but a data frame can be returned with \code{FALSE}. 
#' 
#' @param progress Type of progress bar to display. Default is \code{"none"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
google_elevation <- function(latitude, longitude, key, round = NA, vector = TRUE,
                             progress = "none") {
  
  # Build query with arguments
  urls <- stringr::str_c(
    "https://maps.googleapis.com/maps/api/elevation/json?locations=", 
    latitude, ",", longitude, "&key=", key)
   
  # Get elevation data for all queries
  df <- plyr::ldply(urls, google_elevation_worker, .progress = progress)
  
  # Round if desired. 
  if (!is.na(round)) df$elevation <- round(df$elevation, round)
  
  # Get only the elevation vector, not a good object name here
  if (vector) df <- df$elevation
  
  # Return
  df
  
}


# No export
google_elevation_worker <- function(url) {
  
  if (!is.na(url)) {
    
    # Get return
    string <- readLines(url)
    
    # Parse
    json <- fromJSON(string, flatten = TRUE)
    
    # Extract data
    df <- json$results
    
    # Arrange variables
    df <- df %>% 
      select(latitude = location.lat, 
             longitude = location.lng,
             elevation, 
             resolution)
    
  } else {
    
    # An empty data frame
    df <- data_frame(
      latitude = NA, 
      longitude = NA,
      elevation = NA, 
      resolution = NA
    )
    
  }
  
  # Return
  df
  
}
