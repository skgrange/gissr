#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} uses a single file string which does not have to be expanded 
#' and can handle shapefiles with or without specified extensions. GPX files 
#' containing tracks, routes, or waypoints can also be read with \code{sp_read}.
#' 
#' @param file Spatial data file. 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is TRUE. 
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Load a shape file without extension
#' sp.london <- sp_read("london_borough")
#' 
#' # Load a shape file with an extension within a home area
#' sp.london <- sp_read("~/Desktop/london_borough.shp")
#' 
#' # Load a gpx file, no need to define layer
#' sp.hira <- sp_read("~/Desktop/hira_mtb_park.gpx")
#' }
#' 
#' @export
#'
sp_read <- function (file, verbose = TRUE) {
  
  # Expand path
  file <- path.expand(file)
  
  # GPX reader needs a layer
  if (grepl(".gpx$", file, ignore.case = TRUE)) {
    
    # Not really helpful variable names here...
    dir.name <- file
    
    # Layer, in my usage, the most common type of layer
    file.name <- "tracks"
    
  } else {
    
    # Shapefile handling
    # Get file name
    file.name <- basename(file)
    
    # Remove extension(s)
    file.name <- stringr::str_split_fixed(file.name, pattern = "\\.", n = 2)[, 1]
    
    # Get directory
    dir.name <- dirname(file)
    
  }
  
  # Load file with readOGR will work with shapefiles and GPX files with tracks
  suppressWarnings(
    sp <- tryCatch(
      rgdal::readOGR(dir.name, file.name, verbose), 
      error = function(e) NA)
  )
  
  # Try the different layers for GPX files if the first call failed
  # suppressWarnings is due to the is.na() on an S4 class
  # Routes
  suppressWarnings(
    if (grepl(".gpx$", dir.name, ignore.case = TRUE) & is.na(sp)) {
      sp <- tryCatch(rgdal::readOGR(dir.name, "routes", verbose), 
                     error = function(e) NA)
    }
  )
  
  # Waypoints
  suppressWarnings(
    if (grepl(".gpx$", dir.name, ignore.case = TRUE) & is.na(sp)) {
      sp <- tryCatch(rgdal::readOGR(dir.name, "waypoints", verbose), 
                     error = function(e) NA)
    }
  )
  
  suppressWarnings(
    if (is.na(sp)) {
      stop("No spatial data can be found. ")
    }
  )
  
  # Add transform functionality?
  
  # Print projection too
  # cat to keep consistent with rgdal::readOGR
  if (verbose) {
    cat("Projection:", sp_projection(sp), "\n")
  }
    
  # Return
  sp
  
}
