#' Function to export geometries as shapefiles.
#' 
#' \code{write_shapefile} uses \code{rgdal::writeOGR} for the shapefile writer
#' and will write projection information. \code{write_shapefile} will create the
#' directory where the shapefile is to be exported if necessary. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object to be written as a shapefile. 
#' 
#' @param file Filename of shapefile to write. \code{file} will be cleaned to be
#' the "base" filename because many files make up a shapefile. 
#' 
#' @examples 
#' \dontrun{
#' write_shapefile(sp_county, "~/Desktop/county_spatial_data")
#' }
#' 
#' @export
write_shapefile <- function(sp, file) {
  
  # Expand path
  file <- path.expand(file)
  
  # Get directory
  destination <- dirname(file)
  
  # Create directory if it does not exist
  if (!file.exists(destination)) dir.create(destination, recursive = TRUE)
  
  # Get layer which is file name
  layer <- basename(file)
  
  # Remove extension(s)
  layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]

  # Add data slots if not present
  sp <- sp_promote(sp)
  
  # Write files
  rgdal::writeOGR(sp, destination, layer, driver = "ESRI Shapefile",
                  overwrite_layer = TRUE)
  
  # No return
  
}
