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
#' 
#' # Only a vector without messages
#' google_geocode("york minster", verbose = FALSE, wkt = TRUE))
#' 
#' }
#' 
#' @export
google_geocode <- function(input, source = "google", wkt = FALSE, 
                           verbose = TRUE) {
  
  # Catch factors
  if (is.factor(input)) input <- as.character(input)
  
  # Geocode addresses
  if (verbose) {
    
    df <- ggmap::geocode(
      input, 
      source = source, 
      output = "latlona",
      override_limit = TRUE
    )
    
  } else {
    
    suppressWarnings(
      suppressMessages(
        df <- ggmap::geocode(
          input, 
          source = source, 
          output = "latlona",
          override_limit = TRUE
        )
      )
    )
    
  }
  
  if (class(df) == "data.frame") {
    
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
    
  } else {
    
    # Empty data frame
    df <- data.frame()
    
  }
  
  return(df)
  
}
