#' Function to erase pieces of a polygon based on another polygon. 
#' 
#' \code{sp_erase} can be thought as the inverse of \code{sp_punch}. 
#' 
#' @param sp_1 Geometry one. 
#' 
#' @param sp_2 Geometry two, 
#' 
#' @param features Should the function keep features? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_erase <- function(sp_1, sp_2, features = FALSE)
  rgeos::gIntersection(sp_1, sp_2, byid = features)
