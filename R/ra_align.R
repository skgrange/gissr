#' Function to align a raster object with another raster object -- useful when
#' the raster objects' origins are not identical. 
#' 
#' @param ra A raster object to be aligned. 
#' 
#' @param ra_template A raster object used as the template for realignment. 
#'  
#' @author Stuart K. Grange
#' 
#' @return Raster layer. 
#' 
#' @seealso \code{\link{ra_bind}}
#' 
#' @export
ra_align <- function(ra, ra_template) {
  
  # Check projection systems
  stopifnot(sp_projection(ra) == sp_projection(ra_template))
  
  # Use the template to align the raster layer
  # Warning suppression is for warning indicating projection systems are the same
  suppressWarnings(
    ra_aligned <- raster::projectRaster(
      from = ra, to = ra_template, alignOnly = TRUE
    )
  )
  
  # Align the raster layer
  suppressWarnings(
    ra <- raster::projectRaster(ra, to = ra_aligned)
  )
  
  return(ra)
  
}
