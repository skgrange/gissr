#' Functions to export raster objects. 
#' 
#' @param ra Raster object to export.
#' 
#' @param file File to export raster object too. 
#' 
#' @param projection_file Should the projection string also be written? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Invisible, raster object. 
#' 
#' @export
write_geo_tiff <- function(ra, file) {
  
  # Check  
  stopifnot(is.ra(ra))
  
  # Export
  raster::writeRaster(ra, filename = file, format = "GTiff", overwrite = TRUE)
  
  return(invisible(ra))
  
}


#' @rdname write_geo_tiff
#' @export
write_esri_grid_ascii <- function(ra, file, projection_file = FALSE) {
  
  # Check  
  stopifnot(is.ra(ra))
  
  # Export raster object
  raster::writeRaster(ra, filename = file, format = "ascii", overwrite = TRUE)
  
  # Also write projection string
  if (projection_file) 
    writeLines(sp_projection(ra), stringr::str_c(file, ".projection"))
  
  return(invisible(ra))
  
}
