#' Function to bind multiple raster objects together. 
#' 
#' @param ... Raster objects. 
#' 
#' @param tolerance Permissible difference in origin.
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link[raster]{merge}}
#' 
#' @export
ra_bind <- function(..., tolerance = 0.05)
  ra_bind_reduce(list(...), tolerance = tolerance)


ra_bind_reduce <- function(list_ra, tolerance)
  Reduce(function(...) raster::merge(..., tolerance = tolerance), list_ra)
