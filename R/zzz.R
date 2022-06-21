#' Function to squash R check's global variable notes. 
#' 
#' @name zzz
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    "lat", "lon", "changeset", "uid", "visible", "location.lat", "location.lng",
    ".", "address", "elevation", "latitude", "longitude", "resolution", "x", "y",
    "date_sunrise", "date_sunset", "value", "variable", "daylight", "distance",
    "speed", "ID", "cells", "time_lag", "time", "id_sp", "cell", "Time", 
    "altitude", "distance_sum", "level", "raster"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
