#' Function to read \code{GPX} files as tabular data.  
#' 
#' @param file A vector of GPS file names. 
#' 
#' @param transform Should latitude and longitude be used to calculate speed and
#' distance if the file contains the appropriate variables? 
#' 
#' @param .id Optional variable name for file name in the returned tibble.  
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
scrape_gpx <- function(file, transform = TRUE, .id = NULL, verbose = FALSE) {
  
  file %>% 
    purrr::set_names(.) %>% 
    purrr::map_dfr(
      scrape_gpx_worker, transform = transform, verbose = verbose, .id = .id
    )
  
}


scrape_gpx_worker <- function(file, transform, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # Load file as text
  text_gpx <- readr::read_lines(file)
  
  # Parse xml, to-do, find out why a html parser is needed
  xml_tree <- XML::htmlTreeParse(text_gpx, useInternalNodes = TRUE)
  
  # Extended file version? 
  # For garmin watches
  gpx_extended <- any(stringr::str_detect(text_gpx[1:10], "TrackPointExtension"))
  
  # Get variables
  coordinates <- XML::xpathSApply(xml_tree, path = "//trkpt", XML::xmlAttrs)
  
  # Stop here
  if (length(coordinates) == 0L) {
    stop(
      "GPX file does not contain coordinates in a standard location.", 
      call. = FALSE
    )
  }
  
  # Get latitude and longitude
  latitude <- coordinates["lat", ] %>% 
    as.numeric()
  
  longitude <- coordinates["lon", ] %>% 
    as.numeric()
  
  elevation <- xml_tree %>% 
    XML::xpathSApply(path = "//trkpt/ele", XML::xmlValue) %>% 
    as.numeric()
  
  # When there is no elevation data
  if (length(elevation) == 0L) elevation <- NA_real_
  
  date <- xml_tree %>% 
    XML::xpathSApply(path = "//trkpt/time", XML::xmlValue) %>% 
    lubridate::ymd_hms(tz = "UTC")

  # Extended variables for garmin watches
  if (gpx_extended) {
    
    heart_rate <- xml_tree %>% 
      XML::xpathSApply(path = "//hr", XML::xmlValue) %>%
      as.numeric()
    
    cadence <- xml_tree %>% 
      XML::xpathSApply(path = "//cad", XML::xmlValue) %>% 
      as.numeric()
    
  }

  if (length(date) != 0) {
    
    # Build tibble
    df <- tibble(date, elevation, latitude, longitude)
    
    # Add extensions too
    if (gpx_extended) {
      df <- mutate(df, heart_rate = !!heart_rate, cadence = !!cadence)
    }
    
    # Calculate things, needs date
    if (transform) {
      df <- df %>% 
        mutate(time_elapsed = date - min(date),
               time_elapsed = hms::as_hms(time_elapsed),
               distance = distance_by_haversine(latitude, longitude),
               time_lag = threadr::lag_delta(as.numeric(date)),
               speed = distance / time_lag,
               speed_km_h = threadr::ms_to_km_h(speed)) %>% 
        select(-time_lag)
    }
    
  } else {
    # For when the gpx file does not contain a date variable
    df <- tibble(elevation, latitude, longitude)
  }
  
  return(df)
  
}
