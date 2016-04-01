#' Function to "punch" (add/insert) a hole in a polygon. 
#' 
#' Punching could also be called a reverse-clip or an erasing process. 
#' 
#' @param sp_base SpatialPolygons which will have a hole punched into it. 
#' 
#' @param sp_hole SpatialPolygons which will be represented as a hole in 
#' \code{sp_base}. 
#' 
#' @param features Should \code{sp_punch} be applied across all features in
#' \code{sp_base}? 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Load coastline
#' sp_uk <- sp_read("uk_coastline")
#' 
#' # Load a drawn polygon
#' data_drawn <- read.delim("drawn_polygon.txt")
#' 
#' # Promote to spatial
#' sp_drawn <- data_frame_to_polygon(data_drawn)
#' 
#' # Punch a hole in the coastline object
#' sp_uk_punch <- sp_punch(sp_uk, sp_drawn)
#' 
#' # Check
#' plot(sp_uk_punch)
#' 
#' }
#' 
#' @export
sp_punch <- function(sp_base, sp_hole, features = FALSE)
  rgeos::gDifference(sp_base, sp_hole, byid = features)
