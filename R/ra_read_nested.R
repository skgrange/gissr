#' Function to read raster files within a nested tibble. 
#' 
#' @param file Vector of file names containing raster data.
#' 
#' @param forify Should the raster data also be loaded (or converted/fortified) 
#' into a tabular form? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Nested tibble grouped by rows. 
#' 
#' @export
ra_read_nested <- function(file, forify = FALSE) {
  
  # Read variables and levels
  df <- ra_read_variables(file)
  
  # Read the raster data within a nested tibble
  df <- df %>% 
    rowwise(file,
            level,
            variable) %>% 
    mutate(raster = list(raster::brick(file, varname = variable, level = level)))
  
  # Format raster data as tabular data too
  if (forify) {
    df <- mutate(df, raster_fortify = list(ra_forify(raster)))
  }
  
  return(df)
  
}
