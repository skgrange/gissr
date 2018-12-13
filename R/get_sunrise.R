#' Function to calculate sunrise and sunset dates and times for a location. 
#' 
#' \code{get_sunrise} uses \code{\link{sunriset}} for the calculation of dates
#' and times. 
#' 
#' @param latitude Latitude of a location. 
#' 
#' @param longitude Longitude of a location. 
#' 
#' @param start Start date.
#' 
#' @param end End date.
#'
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{sunriset}}
#' 
#' @examples 
#' 
#' # Get sunrise and sunset dates for London for today
#' get_sunrise(latitude = 51.5072, longitude = 0.1275)
#' 
#' # Or specify dates
#' get_sunrise(
#'   latitude = 51.5072, 
#'   longitude = 0.1275, 
#'   start = "2015-12-01", 
#'   end = "2015-12-31"
#' )
#'
#' @export
get_sunrise <- function(latitude, longitude, start = NA, end = NA) {
  
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
  
  # Build data frame and add extras
  df <- data_frame(
    date, 
    latitude, 
    longitude, 
    date_sunrise = sunrise, 
    date_sunset = sunset
  ) %>% 
    mutate(daylight = as.numeric(date_sunset) - as.numeric(date_sunrise),
           sunrise = date_to_hms(sunrise),
           sunset = date_to_hms(sunset))

  return(df)
  
}


date_to_hms <- function(x) {
  
  # x %>% 
  #   stringr::str_split_fixed(" ", 2) %>% 
  #   .[, 2] %>% 
  #   hms::as.hms()
  
  x <- as.numeric(x) - as.numeric(lubridate::floor_date(x, "day")) 
  x <- hms::as.hms(x)
  return(x)
  
}
