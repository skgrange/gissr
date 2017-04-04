#' Function to find latitude and longitude pairs from an address-string using 
#' Google Maps API.
#' 
#' The Google Maps API is not open-source, therefore check the licensing 
#' conditions for usage conditions 
#' (\url{https://developers.google.com/maps/terms}). Users of this function 
#' must use the geocoded data to display on a Google Map. 
#' 
#' \code{google_geocode} is a wrapper for \code{ggmap::geocode} with some simple 
#' enhancements to clean the output and avoid messages to the console. Other 
#' address elements such as postcodes can also be transformed successfully. 
#'
#' @param input The input to transform to latitude and longitude pairs.
#' 
#' @param source What API should \code{google_geocode} access? Options are 
#' "google" or "dsk" and the default is "google". 
#' 
#' @param override_limit Should the function attempt to override the 2500 queries
#' a day limit? This does not always work. 
#' 
#' @param wkt Should the return be a vector of WKT (well known text)? This is 
#' useful for when transforming a data frame. 
#' 
#' @param verbose Should the functions give messages?
#'
#' @author Stuart K. Grange
#' 
#' @return Data frame or character vector. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Return data frame
#' google_geocode("bath abbey")
#' string     address                                                    latitude longitude
#' bath abbey bath abbey, bath, bath and north east somerset ba1 1lt, uk 51.38148 -2.358735
#' 
#' # Only a vector
#' google_geocode("york minster", verbose = FALSE, wkt = TRUE)
#' POINT (-1.0819205 53.9623292)
#' 
#' }
#'
#' @import dplyr
#' @export
google_geocode <- function(input, source = "google", override_limit = TRUE,
                           wkt = FALSE, verbose = TRUE) {
  
  # Catch factors
  if (is.factor(input)) input <- as.character(input)
  
  # Geocode addresses
  if (verbose) {
    
    df <- ggmap::geocode(input, source = source, output = "latlona")
    
  } else {
    
    suppressMessages(
      df <- ggmap::geocode(input, source = source, output = "latlona")
    )
    
  }
  
  # Add if missing
  if (!"address" %in% names(df)) df$address <- NA
  
  # Rename and add input string
  df <- df %>% 
    mutate(input = input) %>% 
    select(input, 
           address_geocode = address, 
           latitude = lat, 
           longitude = lon)
  
  # Make a vector, not a good name here
  if (wkt) df <- stringr::str_c("POINT (", df$longitude, " ", df$latitude, ")")
  
  # Return
  df
  
}
