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
#' @return Data frame.
#' 
#' @seealso \code{\link{sunriset}}
#' 
#' @examples 
#' 
#' \dontrun{
#' # Get sunrise and sunset dates for London for the current year
#' get_sunrise(latitude = 51.5072, longitude = 0.1275)
#' 
#' # Or specify dates
#' get_sunrise(latitude = 51.5072, longitude = 0.1275, start = "2015-12-01", 
#'   end = "2015-12-31")
#' 
#' }
#'
#' @export
get_sunrise <- function(latitude, longitude, start = NA, end = NA) {
  
  # Make spatial points, assumes latitude and longitude
  sp <- sp_from_data_frame(
    data.frame(
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
  sunrise <- maptools::sunriset(sp, date, direction = "sunrise", POSIXct.out = TRUE)
  sunrise <- sunrise[, 2]
  
  # Calculate sunset for dates
  sunset <- maptools::sunriset(sp, date, direction = "sunset", POSIXct.out = TRUE)
  sunset <- sunset[, 2]
  
  # Build data frame
  df <- data.frame(
    date, 
    latitude, 
    longitude, 
    date_sunrise = sunrise, 
    date_sunset = sunset
  )
  
  # Add the extras
  df$daylight <- difftime(df$date_sunset, df$date_sunrise, units = "hours")
  df$daylight <- as.numeric(df$daylight)
  # df$daylight <- round(df$daylight, 3)
  
  # Get times
  df$time_sunrise <- stringr::str_split_fixed(df$date_sunrise, " ", 2)[, 2]
  df$time_sunrise <- lubridate::parse_date_time(df$time_sunrise, "HMS")
  
  df$time_sunset <- stringr::str_split_fixed(df$date_sunset, " ", 2)[, 2]
  df$time_sunset <- lubridate::parse_date_time(df$time_sunset, "HMS")
  
  return(df)
  
}
