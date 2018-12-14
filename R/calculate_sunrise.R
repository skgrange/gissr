#' Function to calculate sunrise and sunset dates and times for a location. 
#' 
#' \code{calculate_sunrise} uses \code{\link{sunriset}} for the calculation of
#' dates and times. 
#' 
#' @param latitude Latitude of a location. 
#' 
#' @param longitude Longitude of a location. 
#' 
#' @param start Start date, if not used, the default is the system's date. 
#' 
#' @param end End date, if not used, the default is the system's date. 
#' 
#' @param tz Time zone to conduct calculations in.
#' 
#' @param ... A construct to allow for absorption of additional arguments, 
#' useful when using function with \strong{purrr}. 
#'
#' @author Stuart K. Grange.
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{sunriset}}
#' 
#' @examples 
#' 
#' # Get sunrise and sunset dates for London for today
#' calculate_sunrise(latitude = 51.5072, longitude = 0.1275)
#' 
#' # Or specify dates
#' calculate_sunrise(
#'   latitude = 51.5072, 
#'   longitude = 0.1275, 
#'   start = "2015-12-01", 
#'   end = "2015-12-31"
#' )
#'
#' @export
calculate_sunrise <- function(latitude, longitude, start = NA, end = NA, 
                                     tz = "UTC", ...) {
  
  # Catch NA time zones
  tz <- if_else(is.na(tz), "UTC", tz)
  
  # Make spatial points, assumes latitude and longitude
  sp <- sp_from_data_frame(
    data_frame(
      latitude, 
      longitude
    ), 
    type = "points"
  )
  
  # Catch dates
  if (is.na(start)) start <- Sys.Date()
  if (is.na(end)) end <- Sys.Date()
  
  # Parse
  start <- lubridate::ymd(start, tz = "UTC")
  end <- lubridate::ymd(end, tz = "UTC")
  
  # Catch again
  if (start > end) end <- start
  
  # Create date sequence
  date <- seq(start, end, "day")
  
  # Calculate sunrise for dates
  sunrise <- maptools::sunriset(
    crds = sp, 
    dateTime = date, 
    direction = "sunrise", 
    POSIXct.out = TRUE
  )[, 2]
  
  # Calculate sunset for dates
  sunset <- maptools::sunriset(
    crds = sp, 
    dateTime = date, 
    direction = "sunset", 
    POSIXct.out = TRUE
  )[, 2]
  
  # Change time zones
  if (tz != "UTC") {
    
    sunrise <- lubridate::with_tz(sunrise, tzone = tz)
    sunset <- lubridate::with_tz(sunset, tzone = tz)
    
  }
  
  # Build data frame and add extras
  df <- data_frame(
    date, 
    latitude, 
    longitude, 
    date_sunrise = sunrise, 
    date_sunset = sunset
  ) %>% 
    mutate(daylight = as.numeric(date_sunset) - as.numeric(date_sunrise),
           daylight = hms::as.hms(daylight),
           sunrise = date_to_hms(sunrise),
           sunset = date_to_hms(sunset))

  return(df)
  
}


date_to_hms <- function(x) {
  
  x <- as.numeric(x) - as.numeric(lubridate::floor_date(x, "day")) 
  x <- hms::as.hms(x)
  return(x)
  
}
