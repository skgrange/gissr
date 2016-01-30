#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} will expand file paths and make a "best-guess" on what layer 
#' is to be loaded for spatial data files. 
#' 
#' @param file Spatial data file name. For shapefiles and Mapinfo TAB files, a
#' file extension is optional. 
#' 
#' @param layer Layer within \code{file} to read. Default is \code{NULL} and 
#' should not be needed for shapefiles, Mapinfo TAB files, and GeoJSON files. 
#' Default behavior for GPX files is to attempt to read the \code{"tracks"} 
#' layer. 
#' 
#' @param geom An override for when spatial data files contain more than one 
#' geometry type which is commonly found in GeoJSON and KML files. \code{geom} 
#' can be one of: \code{"points"}, \code{"lines"}, or \code{"polygons"}. 
#' 
#' @param lower Should the names of the data slot be forced to be lower case? 
#' Note that the default is \code{TRUE}.
#' 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{readOGR}}, \code{\link{ogrInfo}}, \code{\link{ogrDrivers}}
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
#' }
#' 
#' @export
sp_read <- function (file, layer = NULL, geom = NULL, lower = TRUE, verbose = TRUE) {
  
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
  if (!is.null(layer) & grepl(".gpx$", file, ignore.case = TRUE)) {
    layer <- ifelse(layer %in% c("point", "points"), "waypoints", layer)
  }
  
  # Switch for geom type
  if (!is.null(geom)) {
    geom <- stringr::str_to_lower(geom)
    geom <- ifelse(geom %in% c("point", "points"), "wkbPoint", geom)
    geom <- ifelse(geom %in% c("line", "lines"), "wkbLineString", geom)
    geom <- ifelse(geom %in% c("polygon", "polygons"), "wkbPolygon", geom)
  }
  
  # Read file with rgdal
  sp <- rgdal::readOGR(file, layer, require_geomType = geom, verbose)
  
  # Lower case names for data slot
  if (lower & grepl("data", class(sp), ignore.case = TRUE)) {
    names(sp@data) <- tolower(names(sp@data))
  }
  
  # Remove NA variables
  
  # Message projection string
  if (verbose) message(sp_projection(sp), call. = FALSE)
  
  # Return
  sp
  
}


# sp_read <- function (file, verbose = TRUE, clean = TRUE, unit = NULL, geom = NULL) {
#   
#   # Expand path
#   file <- path.expand(file)
#   
#   # GPX, geojson, and kml handing, needs a generic layer string
#   if (grepl(".gpx$|json$|.kml$", file, ignore.case = TRUE)) {
#     # Not really a helpful variable name here...
#     destination <- file
#     
#     # GPX layer, in my usage, the most common type of layer
#     if (grepl(".gpx$", file, ignore.case = TRUE)) layer <- "tracks"
#     
#     # GeoJSON layer
#     if (grepl("json$", file, ignore.case = TRUE)) layer <- "OGRGeoJSON"
#     
#     # KML layer
#     if (grepl("kml$", file, ignore.case = TRUE)) {
#       layer <- basename(destination)
#       
#       # Remove extension(s)
#       layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]
#       
#     }
#   
#   # Shapefile and mapinfo handling
#   } else {
#     # Get layer which is file name
#     layer <- basename(file)
#     
#     # Remove extension(s)
#     layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]
#     
#     # Get directory
#     destination <- dirname(file)
#     
#   }
#   
#   
#   # Load file with readOGR will work with shapefiles, geojson, GPX files with
#   # tracks, and mapinfo files
#   sp <- tryCatch({
#     rgdal::readOGR(destination, layer, verbose)
#     
#     }, warning = function (w) {
#       # Do not print warning
#       
#     }, error = function (e) {
#       # Print warning
#       cat(e$message)
#       # Assignment
#       NULL
#   })
#   
#   
#   # Try the different layers for GPX files if the first call failed
#   if (grepl(".gpx$", destination, ignore.case = TRUE) & is.null(sp)) {
#     
#     # Try routes
#     sp <- tryCatch({
#       rgdal::readOGR(destination, layer = "routes", verbose)
#       
#     }, warning = function (w) {
#       # Do not print warning
#       
#     }, error = function (e) {
#       # Print warning
#       cat(e$message)
#       # Assignment
#       NULL
#     })
#     
#     # Try waypoints
#     if (is.null(sp)) {
#       
#       sp <- tryCatch({
#         rgdal::readOGR(destination, layer = "waypoints", verbose)
#         
#       }, warning = function (w) {
#         # Do not print warning
#         
#       }, error = function (e) {
#         # Print warning
#         cat(e$message)
#         # Assignment
#         NULL
#       })
#       
#     }
#     
#   }
#   
#   
#   # GeoJSON files with multiple geom types
#   if (grepl("json$", destination, ignore.case = TRUE) & is.null(sp)) {
#     
#     sp <- tryCatch({
#       rgdal::readOGR(destination, layer, verbose,  require_geomType = geom)
#       
#     }, warning = function (w) {
#       # Do not print warning
#       
#     }, error = function (e) {
#       # Print warning
#       cat(e$message)
#       # Assignment
#       NULL
#     })
#     
#   }
#   
#   
#   # KML files
#   if (grepl(".kml$", destination, ignore.case = TRUE) & is.null(sp)) {
#     
#     sp <- tryCatch(
#       suppressWarnings(
#         rgdal::readOGR(destination, layer, verbose, require_geomType = geom)
#       ),
#       error = function (e) NULL)
#     
#     # Use override
#     if (is.null(sp)) {
#       sp <- tryCatch(
#           rgdal::readOGR(destination, layer = unit, verbose, require_geomType = geom), 
#           error = function (e) {
#             
#             cat(e$message)
#             NULL
#             
#           })
#       
#     }
#     
#   }
#   
#   # Message
#   if (is.null(sp)) stop("No spatial data found.", call. = FALSE)
# 
#   # Add transform functionality?
#   
#   # Print projection too
#   # cat to keep consistent with readOGR
#   if (verbose) cat("Projection:", sp_projection(sp), "\n")
#   
#   # Lower case names for data slot
#   if (clean & grepl("data", class(sp), ignore.case = TRUE)) {
#     names(sp@data) <- tolower(names(sp@data))
#   }
#   
#   # Return
#   sp
#   
# }
