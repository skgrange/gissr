#' Function to read \code{GPX} files as a data frame from the GPX Logger Android
#' phone app. 
#' 
#' @param file GPX file. 
#' 
#' @param transform Should latitude and longitude be used to calculate speed and
#' distance too? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame.  
#' 
#' @export
scrape_gpx <- function(file, transform = TRUE) {
  
  # Parse xml, to-do, find out why a html parser is needed. 
  xml_tree <- XML::htmlTreeParse(file, useInternalNodes = TRUE)
  
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
  date <- lubridate::ymd_hms(date,tz = "UTC")
  
  # Build data frame
  df <- data.frame(
    file = basename(file),
    date, 
    elevation,
    latitude,
    longitude,
    stringsAsFactors = FALSE
  )
  
  # Calculate things
  if (transform) {
    
    df$distance <- distance_by_haversine(df$latitude, df$longitude)
    df$speed <- df$distance * (df$date - dplyr::lag(df$date))
    df$speed <- as.numeric(df$speed)
    df$speed_km_h <- threadr::ms_to_km_h(df$speed)
    
  }
  
  return(df)
  
}
