#' Function to read gpx files as a data frame from the GPX Logger Android phone
#' app. 
#' 
#' @param file GPX file. 
#' @param transform Should latitude and longitude be used to calculate speed and
#' distance too? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame with correct data types. 
#' 
#' @import XML
#' 
#' @export
scrape_gpx <- function(file, transform = TRUE) {
  
  # Parse xml, to-do, find out why a html parser is needed. 
  xml_tree <- htmlTreeParse(file, useInternalNodes = TRUE)
  
  # Get variables
  coordinates <- xpathSApply(xml_tree, path = "//trkpt", xmlAttrs)
  elevation <- xpathSApply(xml_tree, path = "//trkpt/ele", xmlValue)
  date <- xpathSApply(xml_tree, path = "//trkpt/time", xmlValue)
  # XML::xpathSApply(xml_tree, path = "//trkpt/fix", XML::xmlValue)
  # XML::xpathSApply(xml_tree, path = "//trkpt/sat", XML::xmlValue)
  # XML::xpathSApply(xml_tree, path = "//trkpt/hdop", XML::xmlValue)
  
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
    date, 
    elevation,
    latitude,
    longitude
  )
  
  # Calculate things
  if (transform) {
    
    df$distance <- gissr::distance_by_haversine(df$latitude, df$longitude)
    df$speed <- df$distance * (df$date - dplyr::lag(df$date))
    df$speed <- as.numeric(df$speed)
    df$speed_km_h <- threadr::ms_to_km_h(df$speed)
    
  }
  
  # Return
  df
  
}


#   # Read as text
#   text <- readLines(file, warn = FALSE)
#   
#   # Parse xml
#   xml <- XML::xmlTreeParse(text)
#   
#   # To list
#   list <- XML::xmlToList(xml)
#   
#   # Get list element with the data
#   df <- suppressWarnings(data.frame(t(list[[2]])))
#   
#   # Extract variable which contains a list of coordinates
#   coordinates <- unlist(df$.attrs)
#   
#   # Filter named vectors
#   longitude <- coordinates[ifelse(names(coordinates) == "trkpt.lon", TRUE, FALSE)]
#   latitude <- coordinates[ifelse(names(coordinates) == "trkpt.lat", TRUE, FALSE)]
#   
#   # Build a data frame
#   df_clean <- data.frame(date = unlist(df$time), 
#                          elevation = unlist(df$ele),
#                          latitude,
#                          longitude)
#   
#   # Sort out data types
#   df_clean$date <- lubridate::ymd_hms(df_clean$date, tz = "UTC")
#   df_clean[, -1] <- lapply(df_clean[, -1], function (x) type.convert(as.character(x)))