#' Function to calculate sunrise and sunset dates and times for a location. 
#' 
#' \code{get_sunrise} uses \code{\link{sunriset}} for the calculation of dates
#' and times. 
#' 
#' @return Tidy data frame or pretty printed JSON.
#' 
#' @seealso \code{\link{sunriset}}
#' 
#' @param latitude Latitude of a location. 
#' 
#' @param longitude Longitude of a location. 
#' 
#' @param start Start date.
#' 
#' @param end End date
#' 
#' @param json Should the return be a json string rather than a data frame? 
#' Useful for single observations.
#'
#' @author Stuart K. Grange
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
#' # Or as json
#' get_sunrise(latitude = 51.5072, longitude = 0.1275, json = TRUE)
#' 
#' }
#'
#' @export
get_sunrise <- function(latitude, longitude, start = NA, end = NA, json = FALSE) {
  
  # Make spatial points, assumes latitude and longitude
  sp <- data_frame_to_points(data.frame(latitude, longitude))
  
  # Catch dates
  if (is.na(start)) start <- Sys.Date()
  if (is.na(end)) end <- Sys.Date()
  
  # Parse
  start <- lubridate::ymd(start)
  end <- lubridate::ymd(end)
  
  # Catch again
  if (start > end) end <- start
  
  # Create date sequence
  date <- seq(start, end, "day")
  
  # Calculate sunrise for dates
  sunrise <- maptools::sunriset(sp, date, direction = "sunrise", POSIXct.out = TRUE)
  sunrise <- sunrise[, 2]
  sunrise <- lubridate::round_date(sunrise, "second")
  
  # Calculate sunset for dates
  sunset <- maptools::sunriset(sp, date, direction = "sunset", POSIXct.out = TRUE)
  sunset <- sunset[, 2]
  sunset <- lubridate::round_date(sunset, "second")
  
  # Build data frame
  df <- data.frame(date, 
                   latitude, 
                   longitude, 
                   date_sunrise = sunrise, 
                   date_sunset = sunset)
  
  # Add the extras
  df$daylight <- difftime(df$date_sunset, df$date_sunrise, units = "hours")
  df$daylight <- as.numeric(df$daylight)
  # df$daylight <- round(df$daylight, 3)
  
  # Get times
  df$time_sunrise <- stringr::str_split_fixed(df$date_sunrise, " ", 2)[, 2]
  df$time_sunrise <- lubridate::parse_date_time(df$time_sunrise, "hms")
  
  df$time_sunset <- stringr::str_split_fixed(df$date_sunset, " ", 2)[, 2]
  df$time_sunset <- lubridate::parse_date_time(df$time_sunset, "hms")
  
  if (json) df <- jsonlite::toJSON(df, pretty = TRUE)
  
  # Return
  df
  
}


# Parse dates. This will handle normal and UK locale preference as well as 
# strings or integers which are years. 
#
# No export
parse_date_arguments <- function (date, what) {
  
  # Start of year
  if (what == "start") {
    
    # Catch for when years are used as dates
    if (!is.na(date) & nchar(date) == 4) date <- stringr::str_c(date, "-01-01")
    
    # Round
    date <- ifelse(is.na(date), 
                   as.character(lubridate::floor_date(Sys.Date(), "year")), date)
    
  }
  
  # End of year
  if (what == "end") {
    
    if (!is.na(date) & nchar(date) == 4) date <- stringr::str_c(date, "-12-31")
    
    # Round
    date <- ifelse(is.na(date), 
                   as.character(lubridate::ceiling_date(Sys.Date(), "year")), date)
    
  }
  
  # Parse date
  date <- lubridate::parse_date_time(date, c("ymd", "dmy"))
  
  # Return
  date
  
}
