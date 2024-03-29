#' Function to mask/filter a raster object to a spatial polygon. 
#' 
#' Masking a raster object will filter the object to a spatial polygon with 
#' potentially complicated geometries while \code{\link{ra_crop}} will crop the 
#' object to a rectangular extent. 
#' 
#' @param ra Raster object to be masked/filtered. 
#' 
#' @param sp_polygon Spatial polygon to be used as the filter. 
#' 
#' @param pre_crop Should the raster object be cropped before being masked? Note,
#' the default is \code{TRUE}. 
#' 
#' @param inverse Should an inverse/punch/erase mask be conducted? 
#' 
#' @author Stuart K. Grange.
#' 
#' @return Raster object.
#' 
#' @seealso \code{\link{ra_crop}}
#' 
#' @export
ra_mask <- function(ra, sp_polygon, pre_crop = TRUE, inverse = FALSE) {
  
  # Check the projection systems
  if (!identical(sp_projection(ra), sp_projection(sp_polygon))) {
    stop("Projection systems are not identical.", call. = FALSE)
  }
  
  if (pre_crop) {
    
    # Crop, use extent of polygon here
    ra <- ra_crop(ra, envelope = sp_polygon)
    
    # Sometimes the mask function errors with a raster brick, switch to stack
    if (class(ra) == "RasterBrick") ra <- raster::stack(ra)
    
  }
  
  # Mask
  ra <- raster::mask(ra, sp_polygon, inverse = inverse)
  
  return(ra)
  
}
