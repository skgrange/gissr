#' Function to export GPX files with a time variable. 
#' 
#' @param df Data frame or tibble with at least these three variables: 
#' \code{latitude}, \code{longitude}, and \code{date}. \code{date} also needs 
#' to be of \code{POSIXct} type.
#' 
#' @param file File name of GPX export. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible GPX markup.
#' 
#' @export
write_gpx_with_dates <- function(df, file) {
  
  # Check input
  stopifnot(c("latitude", "longitude", "date") %in% names(df))
  stopifnot(lubridate::is.POSIXt(df$date))
  
  # Format dates for gpx files
  df <- df %>% 
    mutate(date = lubridate::with_tz(date, "UTC"),
           date = format(date, format = "%Y-%m-%dT%H:%M:%OS3Z"))
  
  # Build the body of the gpx file
  gpx_body <- purrr::pmap_chr(df, build_gpx_entry) %>% 
    stringr::str_c(collapse = "\n")
  
  # Combine the different pieces
  gpx_complete <- stringr::str_c(gpx_preamble_tags, gpx_body, gpx_trailing_tags)
  
  # Export to disc
  readr::write_lines(gpx_complete, file)
  
  return(invisible(gpx_complete))
  
}


build_gpx_entry <- function(latitude, longitude, date, ...) {
  
  # Inspired by: 
  # https://stackoverflow.com/questions/51067311/r-convert-gps-to-gpx-with-timestamp
  stringr::str_c(
    '<trkpt lat="', latitude, 
    '" lon="', longitude, 
    '">\n<time>', date, "</time>\n</trkpt>"
  )
  
}


gpx_preamble_tags <- '<?xml version="1.0" encoding="utf-8" standalone="yes"?>
<gpx version="1.1" creator="gissr http://www.gpsvisualizer.com/" 
xmlns="http://www.topografix.com/GPX/1/1" 
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd">
<trk>
<name>gissr export</name>
<trkseg>'

gpx_trailing_tags <- '</trkseg>\n</trk>\n</gpx>'
