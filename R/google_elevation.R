#' Function to find elevation from latitude and longitude coordinate pairs using
#' Google Maps API. 
#' 
#' The Google Maps API is not open-source, therefore check the licensing 
#' conditions for usage conditions 
#' (\url{https://developers.google.com/maps/terms}). Users of this function 
#' must use the elevation data to display on a Google Map. 
#'
#' \code{google_elevation} returns a data frame with the latitude and longitude
#' pairs, elevation estimate, and (horizontal) resolution of elevation estimate. 
#' Coordinate pairs which contain \code{NA} will be returned as \code{NA} 
#' observation. 
#' 
#' @import dplyr
#' 
#' @param latitude Numeric vector of latitude values.
#' 
#' @param longitude Numeric vector of longitude values.
#' 
#' @param key Google Maps API key. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
google_elevation <- function(latitude, longitude, key) {
  
  # Build query with arguments
  suppressWarnings(
    url <- stringr::str_c("https://maps.googleapis.com/maps/api/elevation/json?locations=", 
                          latitude, ",", longitude, "&key=", key)
  )
  
  # Get elevation data for all queries
  # To-do: change this behaviour
  if (length(url) >= 10) {
    
    # With progress bar
    list <- pbapply::pblapply(url, get_elevation)
    
  } else {
    
    # Without progress bar
    list <- lapply(url, get_elevation)
    
  }
  
  # Bind all data frames
  df <- do.call("rbind", list)
  
  # Return
  df
  
}


# Function which interacts with the api
# No export
get_elevation <- function(url) {
  
  # Query url and get a json object if not NA
  if (!is.na(url)) {
    
    string <- readLines(url)
    string <- stringr::str_c(string, collapse = "")
    
    # Parse json string
    json <- jsonlite::fromJSON(string)
    
    # Extract data
    df <- json$results
    df <- jsonlite::flatten(df)
    
    # Arrange variables
    df <- df %>% 
      select(latitude = location.lat, 
             longitude = location.lng,
             elevation, 
             resolution)
    
  } else {
    
    # Data frame full of nas
    df <- data.frame(latitude = NA, 
                     longitude = NA,
                     elevation = NA,
                     resolution = NA)
    
  }
  
  # Return
  df
  
}
