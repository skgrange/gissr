#' Function to read a raster object from file. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file File containing raster data. 
#' 
#' @param variable An optional variable to read within \code{file}. 
#' 
#' @param level What level to load, usually a fourth dimension variable. 
#' 
#' @param in_memory Should the raster object be forced into physical memory? 
#' 
#' @return Raster object. 
#' 
#' @seealso \code{\link[raster]{raster}}
#' 
#' @export
ra_read <- function(file, variable = NA, in_memory = FALSE) {
  
  # Connect to raster object and get a variable
  if (is.na(variable[1])) {
    ra <- raster::raster(file)
  } else {
    ra <- raster::raster(file, varname = variable)
  }
  
  # Ensure raster object is in memory
  if (in_memory) ra <- ra_load(ra)
  
  return(ra)
  
}


#' @rdname ra_read
#' @export
ra_read_brick <- function(file, variable = NA, level = 1, in_memory = FALSE) {
  
  # Connect to raster object and get a variable
  if (is.na(variable[1])) {
    ra <- raster::brick(file, level = level)
  } else {
    ra <- raster::brick(file, varname = variable, level = level)
  }
  
  # Ensure raster object is in memory
  if (in_memory) ra <- ra_load(ra)
  
  return(ra)
  
}
