#' Function to return all values in a raster object. 
#' 
#' @param ra A raster object.
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[raster]{values}}
#' 
#' @return Numeric vector or numeric matrix. 
#' 
#' @examples 
#' 
#' # Load raster object
#' ra_example <- raster::raster(system.file("external/test.grd", package = "raster"))
#' 
#' # Get values
#' ra_values(ra_example)
#' 
#' # Get unique values and sort
#' sort(unique(ra_values(ra_example)))
#' 
#' @export
ra_values <- function(ra) raster::values(ra)
