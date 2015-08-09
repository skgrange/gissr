#' Function to find latitude and longitude pairs from an address-string using 
#' Google Maps.
#' 
#' The Google Maps API is not open-source, therefore check the licensing 
#' conditions for usage conditions 
#' (\url{https://developers.google.com/maps/terms}). 
#' 
#' \code{google_geocode} is a wrapper for \code{ggmap::geocode} with some simple 
#' enhancements to clean the output and avoid messages to the console. Other 
#' address elements such as postcodes can also be transformed sucessfully. 
#'
#' @param string The address string to transform to latitude and longitude pairs.
#' @param override_limit Should the function attempt to override the 2500 queries
#' a day limit? This does not always work. 
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' google_geocode("bath abbey")
#' string     address                                                    latitude longitude
#' bath abbey bath abbey, bath, bath and north east somerset ba1 1lt, uk 51.38148 -2.358735
#' }
#'
#' @export
#'
google_geocode <- function (string, override_limit = TRUE) {
  
  # Catch factors
  if (is.factor(string)) {
    string <- as.character(string)
  }
  
  # Do not allow function to message the screen
  suppressMessages(
    # Do not warn about un-matchable string
    suppressWarnings(
      # Do not stop if string cannot be matched
      df <- tryCatch(
        # Get latitude and longitude as data frame
        ggmap::geocode(string, output = "latlona", 
                       override_limit = override_limit),
        error = function(e) data.frame(lon = NA, lat = NA, address = NA)
      )
    )
  )
  
  # Create tidy data
  df <- data.frame(string = string, address = df$address, latitude = df$lat, 
                   longitude = df$lon)
  
  # Return
  df
  
}
