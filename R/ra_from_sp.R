#' Function to create a raster object from a spatial (vector) object. 
#' 
#' @param sp Spatial object. 
#' 
#' @param resolution Resolution of the returned raster object.  
#' 
#' @param drop_index Should the index be dropped from the raster return? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster layer or brick. 
#' 
#' @seealso \code{\link{rasterize}} 
#' 
#' @export
ra_from_sp <- function(sp, resolution, drop_index = TRUE) {
  
  # Create blank raster
  ra_blank <- ra_create(sp, resolution)
  
  # Rasterise sp
  ra <- raster::rasterize(sp, ra_blank)
  
  # Drop the index
  if (drop_index) ra <- ra[[-1]]
  
  return(ra)
  
}


ra_create <- function(sp, resolution) {
  
  # Get extent of spatial object
  ra_extent <- raster::extent(sp)
  
  # Create a blank raster layer
  ra <- raster::raster(
    ra_extent, 
    crs = sp_projection(sp), 
    res = resolution
  )
  
  return(ra)
  
}
