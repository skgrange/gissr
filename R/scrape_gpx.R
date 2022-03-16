#' Function to read \code{GPX} files as tabular data.  
#' 
#' @param file A vector of GPS file names. 
#' 
#' @param transform Should latitude and longitude be used to calculate speed and
#' distance if the file contains the appropriate variables? 
#' 
#' @param creator Should the \code{creator} variable be extracted from the file? 
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
scrape_gpx <- function(file, transform = TRUE, creator = FALSE, .id = NULL, 
                       verbose = FALSE) {
  
  file %>% 
    purrr::set_names(.) %>% 
    purrr::map_dfr(
      scrape_gpx_worker, 
      transform = transform, 
      creator = creator,
      verbose = verbose, 
      .id = .id
    )
  
}


scrape_gpx_worker <- function(file, transform, creator, verbose) {
  
  # Message to user
  if (verbose) message(threadr::date_message(), "`", file, "`...")
  
  # Load file as text
  text_gpx <- readr::read_lines(file, progress = FALSE)
  
  # Scrape creator from file
  if (creator) {
    creator_string <- extract_creator_string(text_gpx)
  } else {
    creator_string <- NULL
  }
  
  # Parse xml, to-do, find out why a html parser is needed
  xml_tree <- XML::htmlTreeParse(text_gpx, useInternalNodes = TRUE)
  
  # Extended file version? 
  # For garmin watches
  gpx_extended <- any(stringr::str_detect(text_gpx[1:10], "TrackPointExtension"))
  gpx_extended <- if_else(is.na(gpx_extended), FALSE, TRUE)
  
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
    
    # Catches if the extended gps values do not contain such values
    if (length(heart_rate) == 0) heart_rate <- NA_real_
    if (length(cadence) == 0) cadence <- NA_real_
    
  }

  if (length(date) != 0) {
    
    # Build tibble
    if (creator) {
      df <- tibble(creator = creator_string, date, elevation, latitude, longitude)
    } else {
      df <- tibble(date, elevation, latitude, longitude)
    }
    
    # Add extensions too
    if (gpx_extended) {
      df <- mutate(df, heart_rate = !!heart_rate, cadence = !!cadence)
    }
    
    # Calculate things, needs date
    if (transform) {
      df <- df %>% 
        mutate(time = date - min(date),
               time = hms::as_hms(time),
               distance = distance_by_haversine(latitude, longitude),
               distance = if_else(is.na(distance), 0, distance),
               time_lag = threadr::lag_delta(as.numeric(date)),
               speed = distance / time_lag,
               distance = cumsum(distance)) %>% 
        select(-time_lag)
    }
    
  } else {
    # For when the gpx file does not contain a date variable
    df <- tibble(elevation, latitude, longitude)
  }
  
  return(df)
  
}


extract_creator_string <- function(text, n = 10) {
  
  # Filter string
  creator_string <- stringr::str_subset(text[1:n], "creator")
  
  # Get locations
  index_creator <- stringr::str_locate(creator_string, "creator")[1]
  index_version <- stringr::str_locate(creator_string, "version")[1]
  
  # A location switch based on order
  if (stringr::str_detect(creator_string, "^<gpx creator")) {
    # For garmin exports
    index_creator_location <- 2L
  } else {
    index_creator_location <- if_else(index_creator < index_version, 3L, 4L)
  }
  
  # Format
  if (index_creator == 1L) {
    
    creator_string <- creator_string %>% 
      stringr::str_remove_all('creator|=|"') %>% 
      stringr::str_trim()
    
  } else {
    
    # Split and format
    creator_string <- creator_string %>% 
      stringr::str_split_fixed("creator|version|xmlns", n = 5) %>% 
      .[, index_creator_location] %>% 
      stringr::str_remove_all('=|"') %>% 
      stringr::str_trim()
    
  }
  
  return(creator_string)
  
}
