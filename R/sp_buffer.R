#' Function to include or exclude an area of a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param features Should the geometries of all features within \code{sp} be
#' buffered? Default is \code{TRUE}. 
#' 
#' @param width Width of buffer. \code{width}'s units is in \code{sp}'s 
#' projection system.
#' 
#' @param end_style End style of the buffered geometry, one of \code{"round"}, 
#' \code{"flat"}, or \code{"square"}. 
#' 
#' @return Spatial object. 
#' 
#' @export
sp_buffer <- function(sp, features = TRUE, width = 1, end_style = "round") {
  
  # Parse
  end_style <- stringr::str_to_upper(end_style)
  stopifnot(end_style %in% c("ROUND", "FLAT", "SQUARE"))
  
  # Warning suppression for when non-metric projections used and message 
  # suppression is for rgeos retirement message
  sp <- suppressWarnings(
    suppressMessages(
      rgeos::gBuffer(sp, byid = features, width = width, capStyle = end_style) 
    )
  )
  
  return(sp)
  
}
