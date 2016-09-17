#' Function to create spatial objects from JavaScript arrays. 
#' 
#' JavaScript arrays take the form of:
#' \code{[[54.35221,-0.88518],[54.35237,-0.88544]]}.
#' 
#' @param file File, url, or character vector containing a JavaScript array. 
#' @param type Spatial data type. Default is \code{"points"}. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_from_js_array <- function(text, type = "points", 
                             projection = projection_wgs84()) {

  # Do the string processing
  text_clean <- stringr::str_replace_all(text, " ", "")
  text_clean <- stringr::str_replace_all(text_clean, ";$", "")
  text_clean <- stringr::str_replace_all(text_clean, ",]$", "]")
  
  # Parse
  matrix <- jsonlite::fromJSON(text_clean)

  # Build data frame
  df <- data.frame(
    latitude = matrix[, 1],
    longitude = matrix[, 2],
    stringsAsFactors = FALSE
  )
  
  # Promote
  sp <- sp_from_data_frame(df, type = type, projection = projection)
  
  # Demote
  sp <- sp_demote(sp)
  
  # Return
  sp
  
}
