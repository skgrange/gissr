#' Function to create spatial objects from JavaScript arrays. 
#' 
#' JavaScript arrays take the form of:
#' \code{[[54.35221,-0.88518],[54.35237,-0.88544]]}.
#' 
#' @param text Character vector, file, or url containing a JavaScript array. 
#' 
#' @param type Spatial data type. Default is \code{"points"}. 
#' 
#' @param projection Projection string. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Spatial object. 
#' 
#' @export
sp_from_js_array <- function(text, type = "points", 
                             projection = projection_wgs84()) {
  
  # Vectorise the worker
  list_sp <- purrr::map(text, sp_from_js_array_worker, type = type)
  
  # Bind objects
  sp <- sp_bind(list_sp)
  
  # Drop data slot
  sp <- sp_demote(sp)
  
  # Force projection
  if (!is.na(projection)) sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  return(sp)
  
}


sp_from_js_array_worker <- function(text, type) {

  # Do the string processing
  text_clean <- stringr::str_replace_all(text, " ", "")
  text_clean <- stringr::str_replace_all(text_clean, ";$", "")
  text_clean <- stringr::str_replace_all(text_clean, ",]$", "]")
  
  # Parse
  matrix <- jsonlite::fromJSON(text_clean)

  # Build tibble
  df <- tibble(
    latitude = matrix[, 1],
    longitude = matrix[, 2]
  )
  
  # Promote to spatial data
  sp <- sp_from_data_frame(df, type = type, projection = NA)
  
  return(sp)
  
}
