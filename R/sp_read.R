#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} uses a single file-string which does not have to be expanded.
#' 
#' \code{sp_read} can read: 
#' \itemize{
#'   \item Shapefiles with or without specified extensions. 
#'   \item GPX files containing tracks, routes, or waypoints. 
#'   \item GeoJSON files. 
#'   \item Mapinfo TAB files with or without specified extensions. 
#' }
#' 
#' @param file Spatial data file. 
#' 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is \code{TRUE}. 
#' 
#' @param tolower Should the names of the data slot be forced to be lower case? 
#' Note that the default is \code{TRUE}.
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
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
sp_read <- function (file, verbose = TRUE, tolower = TRUE) {
  
  # Expand path
  file <- path.expand(file)
  
  # GPX and geojson handing, needs a generic layer string
  if (grepl(".gpx$|json$", file, ignore.case = TRUE)) {
    
    # Not really a helpful variable name here...
    destination <- file
    
    # GPX layer, in my usage, the most common type of layer
    if (grepl(".gpx$", file, ignore.case = TRUE)) {
      layer <- "tracks"
    }
    
    # GeoJSON layer
    if (grepl("json$", file, ignore.case = TRUE)) {
      layer <- "OGRGeoJSON"
    }
    
  # Shape file and mapinfo handling
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
  suppressWarnings(
    sp <- tryCatch(
      rgdal::readOGR(destination, layer, verbose), 
      error = function(e) NA)
  )
  
  # Try the different layers for GPX files if the first call failed
  # suppressWarnings is due to the is.na() on a S4 class
  # Routes
  suppressWarnings(
    if (grepl(".gpx$", destination, ignore.case = TRUE) & is.na(sp)) {
      sp <- tryCatch(rgdal::readOGR(destination, layer = "routes", verbose), 
                     error = function(e) NA)
    }
  )
  
  # Waypoints
  suppressWarnings(
    if (grepl(".gpx$", destination, ignore.case = TRUE) & is.na(sp)) {
      sp <- tryCatch(rgdal::readOGR(destination, layer = "waypoints", verbose), 
                     error = function(e) NA)
    }
  )
  
  # Message
  suppressWarnings(
    if (is.na(sp)) {
      stop("No spatial data can be found.", call. = FALSE)
    }
  )
  
  # Add transform functionality?
  
  # Print projection too
  # cat to keep consistent with rgdal::readOGR
  if (verbose) {
    cat("Projection:", gissr::sp_projection(sp), "\n")
  }
  
  # Lower case names for data slot
  if (tolower & grepl("data", class(sp), ignore.case = TRUE)) {
    names(sp@data) <- tolower(names(sp@data))
  }
  
  # Return
  sp
  
}
