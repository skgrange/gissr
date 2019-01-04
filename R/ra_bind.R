#' Function to bind multiple raster objects together. 
#' 
#' @param list_ra A list of RasterLayer objects. 
#' 
#' @param tolerance Permissible difference in origin.
#' 
#' @author Stuart K. Grange
#' 
#' @return RasterLayer. 
#' 
#' @seealso \code{\link[raster]{merge}}
#' 
#' @export
ra_bind <- function(list_ra, tolerance = 0.05) {
  
  # Check classes
  classes <- purrr::map_lgl(list_ra, ~class(.) == "RasterLayer")
  
  if (all(classes)) {
    
    # Bind all
    ra <- ra_bind_reduce(list_ra, tolerance = tolerance)
    
  } else{
    
    stop(
      "All items in input need to be of `RasterLayer` data type.", 
      call. = FALSE
    )
    
  }
  
  return(ra)
  
}


ra_bind_reduce <- function(list_ra, tolerance)
  Reduce(function(...) raster::merge(..., tolerance = tolerance), list_ra)
