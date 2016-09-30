#' Function to test if an R object is a Spatial* data-type. 
#' 
#' @param Object to be tested.
#' 
#' @author Stuart K. Grange
#' 
#' @return Logical.
#' 
#' @export
is.sp <- function(x) 
  if (grepl("spatial", sp_class(x), ignore.case = TRUE)) TRUE else FALSE
