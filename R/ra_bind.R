#' Function to bind multiple raster objects together. 
#' 
#' @param list_ra A list of RasterLayer objects. 
#' 
#' @param tolerance Permissible difference in origin.
#' 
#' @param method Method to bind the raster layers together with. The options are
#' "do.call" and "reduce". "reduce" will be faster for two or three list elements
#' while "do.call" is better for large number of elements and is more efficient. 
#' 
#' @param file If method is "do.call", file can be used as the temporary file
#' when the function is binding the elements. 
#' 
#' @author Stuart K. Grange
#' 
#' @return RasterLayer. 
#' 
#' @seealso \code{\link[raster]{merge}}, \code{\link{ra_align}}
#' 
#' @export
ra_bind <- function(list_ra, tolerance = 0.05, method = "do.call", file = NA) {
  
  # Check input
  stopifnot(method %in% c("reduce", "do.call"))
  stopifnot(length(file) == 1)
  
  # Check classes
  classes <- purrr::map_lgl(list_ra, ~class(.) == "RasterLayer")
  
  if (all(classes)) {
    
    if (method == "reduce") {
      ra <- ra_bind_reduce(list_ra, tolerance = tolerance)
    } else if (method == "do.call") {
      ra <- ra_bind_do_call(list_ra, tolerance = tolerance, file = file)
    }
    
  } else{
    stop(
      "All items in input need to be of `RasterLayer` data type.", 
      call. = FALSE
    )
  }
  
  return(ra)
  
}


ra_bind_reduce <- function(list_ra, tolerance) {
  
  Reduce(
    function(...) raster::merge(..., tolerance = tolerance), 
    list_ra
  )
  
}


ra_bind_do_call <- function(list_ra, tolerance, file) {
  
  # Keep things consistent for the raster function
  if (!is.na(file)) file <- ""
  
  # Do the binding with do.call, faster for when elements in list increase
  ra <- do.call(
    raster::merge, 
    c(list_ra, args = list(tolerance = tolerance, filename = file, overwrite = TRUE))
  )
  
  return(ra)
  
}
