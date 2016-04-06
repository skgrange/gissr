#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} will expand file paths and make a "best-guess" on what layer 
#' is to be loaded for spatial data files. Use \code{sp_list_layers} to find what
#' layers a spatial data file contains. 
#' 
#' @param file Spatial data file name. For shapefiles and Mapinfo TAB files, a
#' file extension is optional. For File Geodatabases, an extension is required 
#' (usually \code{.gdb}). 
#' 
#' @param layer Layer within \code{file} to read. Default is \code{NULL} and 
#' should not be needed for shapefiles, Mapinfo TAB files, and GeoJSON files. 
#' Default behaviour for GPX files is to attempt to read the \code{"tracks"} 
#' layer. 
#' 
#' @param geom An override for when spatial data files contain more than one 
#' geometry type in a layer. This is commonly found in GeoJSON and KML files. 
#' \code{geom} can be one of: \code{"points"}, \code{"lines"}, or 
#' \code{"polygons"}. 
#' 
#' @param lower Should the names of the data slot be forced to be lower case? 
#' Note that the default is \code{TRUE}.
#' 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is \code{TRUE}. 
#' 
#' @param drop Should empty variables in the geometries' data frame be dropped? 
#' This is useful for removing useless variables which are commonly found in 
#' \code{GPX} files. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{readOGR}}, \code{\link{sp_list_drivers}}, 
#' \code{\link{sp_list_layers}}
#' 
#' @examples 
#' \dontrun{
#' # Load a shapefile without extension
#' sp_london <- sp_read("london_borough")
#' 
#' # Load a shapefile with an extension within home area
#' sp_london <- sp_read("~/Desktop/london_borough.shp")
#' 
#' # Load a gpx file, no need to define layer
#' sp_hira <- sp_read("~/Desktop/hira_mtb_park.gpx")
#' 
#' # Load GeoJSON file
#' sp_thames_locks <- sp_read("thames_locks.json", verbose = FALSE)
#' 
#' # Load mapinfo file
#' sp_coastline <- sp_read("nz-land-districts.map", verbose = FALSE)
#' 
#' # Geodatabase
#' sp_yemen <- sp_read("world.gdb", layer = "yemen", verbose = FALSE)
#' 
#' }
#' 
#' @export
sp_read <- function(file, layer = NULL, geom = NULL, lower = TRUE, verbose = TRUE,
                    drop = FALSE) {
  
  # Expand path
  file <- path.expand(file)
  
  # Do some guess work for the layer string
  if (is.null(layer)) {
    
    if (grepl("json$", file, ignore.case = TRUE)) {
    
      layer <- "OGRGeoJSON"
      
    } else if (grepl(".gpx$", file, ignore.case = TRUE)) {
    
      # Default to tracks because this is what I use most often
      layer <- "tracks"
      
    } else {
    
      # The best guess for shapefiles and mapinfo files
      # Get layer which is file name
      layer <- basename(file)
      
      # Remove extension(s)
      layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]
      
      # Get directory
      file <- dirname(file)
      
    }
    
  }
  
  # Switch for GPX files
  if (!is.null(layer) & grepl(".gpx$", file, ignore.case = TRUE))
    layer <- ifelse(layer %in% c("point", "points"), "waypoints", layer)

  # Switch for geom type
  if (!is.null(geom)) geom <- parse_geom(geom)
  
  # Read file with rgdal
  sp <- rgdal::readOGR(file, layer, require_geomType = geom, verbose = verbose)
  
  # Message projection string
  if (verbose) cat(sp_projection(sp), "\n")
  
  # Lower case names for data slot
  if (lower & grepl("data", class(sp), ignore.case = TRUE))
    names(sp@data) <- tolower(names(sp@data))
  
  # Remove NA variables, happens often in gpx files
  if (drop) {
    
    # To characters
    sp@data <- threadr::factor_coerce(sp@data)
    # Drop
    sp@data <- sp@data[, colSums(is.na(sp@data)) < nrow(sp@data)]
    
  }
  
  # Return
  sp
  
}


#' Function to list spatial data file drivers on a system. 
#' 
#' A simple wrapper for \code{rgdal::ogrDrivers}. No arguments are used.  
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # List system's drivers
#' sp_list_drivers()
#' }
#' 
#' @export
sp_list_drivers <- function() rgdal::ogrDrivers()


#' Function to list layers within a spatial data file. 
#' 
#' @param file Spatial data file. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # List layers in GeoJSON file, there will only be one
#' sp_list_layers("../../data/points_and_lines.json")
#' }
#' 
#' @export
sp_list_layers <- function(file) rgdal::ogrListLayers(path.expand(file))


#' Function to return layer information within a spatial data file. 
#' 
#' @author Stuart K. Grange
#' 
#' @param file Spatial data file. 
#' 
#' @param layer Layer within \code{file} to read.
#' 
#' @param geom An override for when spatial data files contain more than one 
#' geometry type in a layer. This is commonly found in GeoJSON and KML files. 
#' \code{geom} can be one of: \code{"points"}, \code{"lines"}, or 
#' \code{"polygons"}. 
#' 
#' @examples 
#' \dontrun{
#' # Get infomation of the lines geom within a layer in a GeoJSON file
#' sp_layer_info("../../data/points_and_lines.json", "OGRGeoJSON", 
#'               geom = "lines")
#' }
#' 
#' @export
sp_layer_info <- function(file, layer, geom = NULL) {
  # Expand path
  file <- path.expand(file)
  
  # Switch for geom type
  if (!is.null(geom)) geom <- parse_geom(geom)
  
  info <- rgdal::ogrInfo(file, layer, require_geomType = geom)
  
  # Return
  info
  
}


# A useful swtich 
parse_geom <- function(geom) {

  geom <- stringr::str_to_lower(geom)
  geom <- ifelse(geom %in% c("point", "points"), "wkbPoint", geom)
  geom <- ifelse(geom %in% c("line", "lines"), "wkbLineString", geom)
  geom <- ifelse(geom %in% c("polygon", "polygons"), "wkbPolygon", geom)
  geom
  
}
