#' Function to export geometries as a Geography Markup Language (GML) file. 
#' 
#' \code{write_gml} uses \code{rgdal::writeOGR} for the spatial data writer
#' and will write projection information.
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object to be written as a GML/XML file.  
#' 
#' @param file Filename of GML/XML file. 
#' 
#' @param layer A string which represents the layer of the GML file where the 
#' geometries will be stored. Default uses a generic \code{spatial} string. 
#' 
#' @examples 
#' \dontrun{
#' 
#' # Export polygons
#' write_gml(sp_zones, "~/Desktop/zone_polygons.gml", layer = "zones")
#' 
#' }
#' 
#' @export
write_gml <- function(sp, file, layer = "spatial") {
  
  # Expand path
  file <- path.expand(file)
  
  # Add data slot if not present
  sp <- sp_promote(sp)
  
  # Write file
  rgdal::writeOGR(sp, file, layer = layer, driver = "GML", 
                  overwrite_layer = TRUE)
  
  # No return
  
}
