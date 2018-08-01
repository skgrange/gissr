#' Function to create a raster object from a spatial (vector) object. 
#' 
#' @param sp Spatial object. 
#' 
#' @param resolution Resolution of the returned raster object.  
#' 
#' @param fun Function to determine how multiple observations in a single raster 
#' cell are handled. 
#' 
#' @param na.rm For \code{fun}, should \code{NA}s be ignored? 
#' 
#' @param background Cell's default value when there are no data within the cell. 
#' 
#' @param drop_index Should the index be dropped from the raster return? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Raster layer or raster brick. 
#' 
#' @seealso \code{\link{rasterize}} 
#' 
#' @export
ra_from_sp <- function(sp, resolution, fun = mean, na.rm = TRUE,
                       background = NA, drop_index = TRUE) {
  
  # Create blank raster
  ra_blank <- ra_create(sp, resolution)
  
  # Rasterise sp
  ra <- raster::rasterize(
    x = sp, 
    y = ra_blank, 
    fun = fun, 
    background = background,
    na.rm = na.rm
  )
  
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
