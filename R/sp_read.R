#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} uses a single file string which does not have to be expanded 
#' and can handle shape files with or without specified extensions. GPX files 
#' can also be read with \code{sp_read}. 
#' 
#' @param file Spatial data file. 
#' @param verbose Should information about the data be printed when being 
#' loaded? 
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
#' # Load a gpx file
#' sp.hira <- sp_read("~/Desktop/hira_mtb_park.gpx")
#' }
#' 
#' @export
#'
sp_read <- function (file, verbose = FALSE) {
  
  # Expand path
  file <- path.expand(file)
  
  # Needs to store layer if gpx
  if (grepl(".gpx$", file, ignore.case = TRUE)) {
    
    # Not really helpful names here
    dir.name <- file
    
    # Layer as lines
    file.name <- "tracks"
    
  } else {
    
    # Shapefile stuff
    # Get file name
    file.name <- basename(file)
    
    # Remove extension(s)
    file.name <- stringr::str_split_fixed(file.name, pattern = "\\.", n = 2)[, 1]
    
    # Get directory
    dir.name <- dirname(file)
    
  }
  
  # Load file with readOGR
  sp <- rgdal::readOGR(dir.name, file.name, verbose)
  
  # Add transform functionality?
  
  # Return
  sp
  
}
