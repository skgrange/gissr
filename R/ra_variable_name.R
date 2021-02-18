#' Function to extract the variable name from a raster object. 
#' 
#' @param ra Raster object. 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Character vector with length of 1.  
#' 
#' @export
ra_variable_name <- function(ra) {
  
  # Check input
  stopifnot(is.ra(ra))
  
  # Capture printed output and clean
  x <- capture.output(ra)
  
  # Get the varname
  x_filter <- stringr::str_subset(x, "varname")
  
  # If the varname does not exist, get names
  if (length(x_filter) == 0) {
    x_filter <- stringr::str_subset(x, "names")
  }
  
  # Format the output
  x_filter <- format_ra_variable_output(x_filter)
  
  # If empty make NA
  if (length(x_filter) == 0) x_filter <- NA_character_
  
  return(x_filter)
  
}


format_ra_variable_output <- function(x) {
  
  x %>% 
    stringr::str_split_fixed(":", n = 2) %>% 
    .[, 2] %>% 
    stringr::str_trim()
  
}
