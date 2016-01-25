#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} uses a single file-string which does not have to be expanded.
#' 
#' \code{sp_read} supports: 
#' \itemize{
#'   \item Shapefiles with or without specified extensions. 
#'   \item GPX files containing tracks, routes, or waypoints. 
#'   \item GeoJSON files. 
#'   \item Mapinfo TAB files with or without specified extensions. 
#'   \item Partial support for KML files. 
#' }
#' 
#' @param file Spatial data file. 
#' 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is \code{TRUE}. 
#' 
#' @param clean Should the names of the data slot be forced to be lower case? 
#' Note that the default is \code{TRUE}.
#' 
#' @param unit An override for when a spatial data file has an layer name which
#' is not related to its file name. 
#' 
#' @param geom An override for when spatial data files contain more than one 
#' geometry type; such as KML files. The default is \code{NULL} but use one of 
#' \code{"wkbPoint"}, \code{"wkbLineString"}, or \code{"wkbPolygon"}. See the 
#' \code{require_geomType} argument for the \code{\link{readOGR}} function for 
#' more information.
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
#' # Load a shapefile with an extension within a home area
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
sp_read <- function (file, verbose = TRUE, clean = TRUE, unit = NULL, geom = NULL) {
  
  # Expand path
  file <- path.expand(file)
  
  # GPX, geojson, and kml handing, needs a generic layer string
  if (grepl(".gpx$|json$|.kml$", file, ignore.case = TRUE)) {
    # Not really a helpful variable name here...
    destination <- file
    
    # GPX layer, in my usage, the most common type of layer
    if (grepl(".gpx$", file, ignore.case = TRUE)) layer <- "tracks"
    
    # GeoJSON layer
    if (grepl("json$", file, ignore.case = TRUE)) layer <- "OGRGeoJSON"
    
    # KML layer
    if (grepl("kml$", file, ignore.case = TRUE)) {
      layer <- basename(destination)
      layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]
    }
  
  # Shapefile and mapinfo handling
  } else {
    # Get layer which is file name
    layer <- basename(file)
    
    # Remove extension(s)
    layer <- stringr::str_split_fixed(layer, pattern = "\\.", n = 2)[, 1]
    
    # Get directory
    destination <- dirname(file)
    
  }
  
  # Load file with readOGR will work with shapefiles, geojson, GPX files with
  # tracks, and mapinfo files
#   sp <- tryCatch(
#     rgdal::readOGR(destination, layer, verbose), 
#     error = function (e) message(e$message))
  
  # sp <- try(rgdal::readOGR(destination, layer, verbose))
  
  sp <- tryCatch({
    rgdal::readOGR(destination, layer, verbose)
    
  }, error = function (e) {
    cat(e$message)
    
    # Assign
    NULL
    
  })
  
  # Try the different layers for GPX files if the first call failed
  if (grepl(".gpx$", destination, ignore.case = TRUE) & is.null(sp)) {
    # Routes
    sp <- tryCatch(
      rgdal::readOGR(destination, layer = "routes", verbose), 
      error = function (e) NULL)
    
    # Waypoints
    sp <- tryCatch(
      rgdal::readOGR(destination, layer = "waypoints", verbose), 
      error = function (e) NULL)
    
  }
  
  # KML files
  if (grepl(".kml$", destination, ignore.case = TRUE) & is.null(sp)) {
    # 
    sp <- tryCatch(
      suppressWarnings(
        rgdal::readOGR(destination, layer, verbose, require_geomType = geom)
      ),
      
      error = function (e) NULL)
    
    # Use override
    if (is.null(sp)) {
      sp <- tryCatch(
        suppressWarnings(
          rgdal::readOGR(destination, layer = unit, verbose, require_geomType = geom)
          ),
        error = function (e) NULL)
      
    }
    
  }
  
  # Message
  if (is.null(sp)) stop("No spatial data found.", call. = FALSE)

  # Add transform functionality?
  
  # Print projection too
  # cat to keep consistent with readOGR
  if (verbose) cat("Projection:", sp_projection(sp), "\n")
  
  # Lower case names for data slot
  if (clean & grepl("data", class(sp), ignore.case = TRUE)) {
    names(sp@data) <- tolower(names(sp@data))
  }
  
  # Return
  sp
  
}
