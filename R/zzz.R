#' Function to squash R check's global variable notes. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    "lat", "lon", "changeset", "uid", "visible", "location.lat", "location.lng",
    "address", "elevation", "latitude", "longitude", "resolution", "x", "y"
  )
  
  # Squash the note
  utils::globalVariables(variables)
  
}
