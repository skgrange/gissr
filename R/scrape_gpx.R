#' Function to read \code{GPX} files as a data frame from the GPX Logger Android
#' phone app. 
#' 
#' @param file GPX file. 
#' 
#' @param transform Should latitude and longitude be used to calculate speed and
#' distance if the file contains the appropriate variables? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
scrape_gpx <- function(file, transform = TRUE) {
  
  # Load file as text
  text_gpx <- readr::read_lines(file)
  
  # Parse xml, to-do, find out why a html parser is needed. 
  xml_tree <- XML::htmlTreeParse(text_gpx, useInternalNodes = TRUE)
  
  # Get variables
  coordinates <- XML::xpathSApply(xml_tree, path = "//trkpt", XML::xmlAttrs)
  elevation <- XML::xpathSApply(xml_tree, path = "//trkpt/ele", XML::xmlValue)
  date <- XML::xpathSApply(xml_tree, path = "//trkpt/time", XML::xmlValue)
  
  # Ge latitude and longitude
  latitude <- coordinates["lat", ]
  longitude <- coordinates["lon", ]
  
  # Data types
  elevation <- as.numeric(elevation)
  latitude <- as.numeric(latitude)
  longitude <- as.numeric(longitude)
  date <- lubridate::ymd_hms(date, tz = "UTC")
  
  if (length(date) != 0) {
    
    # Build tibble    
    df <- tibble(date, elevation, latitude, longitude)
    
    # Calculate things, needs date
    if (transform) {
      df <- df %>% 
        mutate(distance = distance_by_haversine(latitude, longitude),
               speed = distance * (date - dplyr::lag(date)),
               speed = as.numeric(speed),
               speed_km_h = threadr::ms_to_km_h(speed))
    }
    
  } else {
    # For when the gpx file does not contain date
    df <- tibble(elevation, latitude, longitude)
  }
  
  return(df)
  
}
