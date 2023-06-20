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
    "altitude", "distance_sum", "level", "raster", "cell_number", "value_index"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}


# To mute proj4 messages during start up
.onLoad <- function(lib, pkg) {
  options("rgdal_show_exportToProj4_warnings"="none")
}


.onAttach <- function(lib, pkg) {
  options("rgdal_show_exportToProj4_warnings"="none")
}


.onUnload <- function(lib, pkg) {
  options("rgdal_show_exportToProj4_warnings"="all")
}
