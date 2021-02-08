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
  x <- ra %>% 
    capture.output() %>% 
    stringr::str_subset("names|varname") %>% 
    stringr::str_split_fixed(":", n = 2) %>% 
    .[, 2] %>% 
    stringr::str_trim()
  
  # If empty make NA
  if (length(x) == 0) x <- NA_character_
  
  return(x)
  
}
