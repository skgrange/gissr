#' Function to test if an R object is a Spatial* data-type. 
#' 
#' @param x Object to be tested.
#' 
#' @author Stuart K. Grange
#' 
#' @return Logical vector with length of 1. 
#' 
#' @export
is.sp <- function(x) 
  if (grepl("spatial", sp_class(x), ignore.case = TRUE)) TRUE else FALSE


#' Function to test if an R object is a Raster* data-type. 
#' 
#' @param x Object to be tested.
#' 
#' @author Stuart K. Grange
#' 
#' @return Logical vector with length of 1. 
#' 
#' @export
is.ra <- function(x)
  if (grepl("raster", class(x), ignore.case = TRUE)) TRUE else FALSE
