#' Function to read spatial data files in a consistent way. 
#' 
#' \code{sp_read} is a wrapper for \code{rgdal::readOGR}, but its usage is the 
#' same as the other file readers in R. Unlike \code{rgdal::readOGR},
#' \code{sp_read} will expand file paths and make a "best-guess" on what layer 
#' is to be loaded for spatial data files. Use \code{sp_list_layers} to find what
#' layers a spatial data file contains. 
#' 
#' \code{sp_read} will also wrap \code{readRDS} if an \code{.rds} file is 
#' detected, but will raise a warning if the file does not contain a spatial 
#' data type. 
#' 
#' @param file Spatial data file name. For shapefiles and Mapinfo TAB files, a
#' file extension is optional. For File Geodatabases, an extension is required 
#' (usually \code{.gdb}). For spatial data types which are composed of a single 
#' file, \code{file} can be a URL and the file will be downloaded into a 
#' temporary directory and then loaded. 
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
#' @param use_iconv Should the input strings attempted to be converted by iconv? 
#' 
#' @param verbose Should information about the data be printed when being 
#' loaded? Default is \code{TRUE}. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{readOGR}}, \code{\link{sp_list_drivers}}, 
#' \code{\link{sp_list_layers}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Load a shapefile without extension
#' sp_london <- sp_read("london_borough")
#' 
#' # Load a shapefile with an extension within home area
#' sp_london <- sp_read("~/Desktop/london_borough.shp")
#' 
#' # Load a gpx file
#' sp_hira <- sp_read("~/Desktop/hira_mtb_park.gpx", type = "tracks")
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
#' # Load a file by using a url
#' file <- "https://bit.ly/2FkUuAP"
#' sp_ridgeway <- sp_read(file, layer = "routes") 
#' 
#' }
#' 
#' @export
sp_read <- function(file, layer = NULL, geom = NULL, lower = TRUE, 
                    use_iconv = FALSE, verbose = TRUE) {
  
  # Download file if a url
  if (grepl("^http:|^https:", file)) {
    
    # Build temp file name
    file_local <- file.path(tempdir(), basename(file))
    
    # Download file
    download.file(file, file_local, quiet = !verbose)
    
    # Switch file name after download
    file <- file_local
    
  } else {
    # Expand path
    file <- path.expand(file)
  }
  
  # If an rds object, just load and return
  if (grepl(".rds$", file, ignore.case = TRUE)) {
    
    # Load rds object
    sp <- readRDS(file)
    
    # Warning to user
    if (!is.sp(sp) && !is.ra(sp)) {
      warning(
        ".rds file has been loaded but it does not contain spatial data...", 
        call. = FALSE
      )
    }
    
    # Return here
    return(sp)
    
  } 
  
  # Do some guess work for the layer string
  if (is.null(layer)) {
    
    # Geojson files
    if (grepl(".geojson$|.json$|.js$", file, ignore.case = TRUE)) {
    
      # File name with no extension
      layer <- basename(file)
      layer <- stringr::str_split_fixed(layer, "\\.", 2)[, 1]
      
    } else if (grepl(".gpx$", file, ignore.case = TRUE)) {
    
      # Default to tracks because this is what I seem to use most often
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
  if (!is.null(geom)) geom <- parse_geom(geom)
  
  # Read file with rgdal
  # Warning suppression is for deprecated function
  suppressWarnings(
    sp <- rgdal::readOGR(
      file, 
      layer, 
      require_geomType = geom,
      use_iconv = use_iconv,
      stringsAsFactors = FALSE,
      verbose = verbose
    )
  )
  
  # Message projection string
  if (verbose) cat(sp_projection(sp), "\n")
  
  # Lower case names for data slot, for me rather than anyone else
  if (lower & grepl("data", class(sp), ignore.case = TRUE)) {
    names(sp@data) <- stringr::str_to_lower(names(sp@data))
  }
  
  # Drop useless and empty variables in gpx file
  if (grepl("data", sp_class(sp), ignore.case = TRUE) &
      grepl(".gpx$", file, ignore.case = TRUE)) {
    
    # Drop NA variables
    sp@data <- drop_na_columns(sp@data)
    
  }
  
  return(sp)
  
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
sp_list_drivers <- function() suppressWarnings(rgdal::ogrDrivers())


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
sp_list_layers <- function(file) {
  suppressWarnings(rgdal::ogrListLayers(path.expand(file)))
}


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
#' @export
sp_layer_info <- function(file, layer, geom = NULL) {
  
  # Expand path
  file <- path.expand(file)
  
  # Switch for geom type
  if (!is.null(geom)) geom <- parse_geom(geom)
  
  suppressWarnings(
    info <- rgdal::ogrInfo(file, layer, require_geomType = geom)
  )
  
  return(info)
  
}


# A useful swtich 
parse_geom <- function(geom) {

  geom <- stringr::str_to_lower(geom)
  geom <- ifelse(geom %in% c("point", "points"), "wkbPoint", geom)
  geom <- ifelse(geom %in% c("line", "lines"), "wkbLineString", geom)
  geom <- ifelse(geom %in% c("polygon", "polygons"), "wkbPolygon", geom)
  return(geom)
  
}


drop_na_columns <- function(df) {
  
  # Test variables for missing-ness
  index <- colSums(is.na(df)) < nrow(df)
  
  # Drop
  df <- df[, index]
  
  # If subsetting has simplified object, make data frame again
  if (class(df) != "data.frame") {
    
    # Make data frame again
    df <- data.frame(df, stringsAsFactors = FALSE)
    
    # Give names
    names(df) <- names(index[index])
    
  }
  
  return(df)
  
}
