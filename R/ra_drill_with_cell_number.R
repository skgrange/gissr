#' Function to `drill' into a raster object and return values based on cell 
#' numbers. 
#' 
#' @param ra Raster object. 
#' 
#' @param cell A vector of cell numbersto extract values for. 
#' 
#' @author Stuart K. Grange. 
#' 
#' @seealso \code{\link[raster]{extract}}, \code{\link{ra_drill}}
#' 
#' @return Tibble. 
#' 
#' @export
ra_drill_with_cell_number <- function(ra, cell) {
  
  # Check inputs
  stopifnot(is.ra(ra))
  stopifnot(is.numeric(cell))
  
  # Extract values from raster object
  x <- ra[cell]
  
  # To tibble
  df <- x %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column("variable") %>% 
    as_tibble()
  
  # Give names, use cell numbers
  names(df)[-1] <- cell
  
  # Make tidy data
  df <- tidyr::gather(df, cell, value, -variable, convert = TRUE)
  
  return(df)
  
}
