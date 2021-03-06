#' Function to parse a data frame containing WKT strings to a spatial object. 
#' 
#' \code{sp_from_wkt} creates geometries from WKT strings and can create spatial
#' data frames from the other variables contained within the input data frame. 
#' \code{sp_from_wkt} is useful after querying a PostGIS database for geometries
#' in WKT format. 
#' 
#' @param df Data frame containing a WKT string variable. \code{df} can also be
#' a vector of WKT strings. 
#' 
#' @param wkt Variable name of WKT strings in \code{df}. Default is \code{"geom"}.
#' 
#' @param data Should variables other than \code{wkt} be added to the spatial 
#' object's data-slot? I.e. create a spatial data frame.
#' 
#' @param keep Should the \code{wkt} be kept in the returned spatial object? 
#' 
#' @param projection A proj4 string to force the projection system after the WKT
#' strings have been parsed. Default is \code{NA}. 
#' 
#' @param verbose Should the function give messages? Useful when large number of
#' WKT strings are to be parsed. Default is \code{FALSE}. 
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Make a spatial object from many wkt strings
#' sp_wkt <- sp_from_wkt(data_wkt, wkt = "geom")
#' 
#' # Make a spatial-data frame object from many wkt strings
#' sp_wkt_with_data <- sp_from_wkt(data_wkt, wkt = "geom", data = TRUE)
#' 
#' }
#' 
#' @export
sp_from_wkt <- function(df, wkt = "geom", data = TRUE, keep = FALSE, 
                        projection = NA, verbose = FALSE) {
  
  # Drop tibble if input is a tibble
  if ("tbl" %in% class(df)) df <- data.frame(df)
  
  # For vectors
  if (class(df) == "character") {
    
    # Vector is input
    wkt_vector <- df
    
    # Vectors will not contain a data slot
    data <- FALSE
    
  } else {
    
    # Check for missing wkt, spatial data cannot be empty
    if (any(is.na(df[, wkt]))) {
      
      # Remove NAs
      df <- df[!is.na(df[, wkt]), ]
      
      # Raise warning
      warning(
        "Missing WKT strings were detected and have been removed...", 
        call. = FALSE
      )
      
    }
    
    # Get a vector of wkt from df
    wkt_vector <- df[, wkt]
    
  }
  
  # Check 
  if (length(wkt_vector) == 0) {
    stop("There are no valid WKT strings.", call. = FALSE)
  }
  
  # Store data
  if (data) {
    
    df_data <- df
    df_data[, wkt] <- NULL
    
  }
  
  # Parse WKT strings
  # Select progress bar type
  if (verbose) progress <- "text" else progress <- "none"
  
  # Message
  if (verbose) message("Parsing WKT...")
  
  # Warning catch is for geoms with negative areas. Why does this occur? 
  suppressWarnings(
    sp_list <- plyr::llply(wkt_vector, rgeos::readWKT, .progress = progress)
  )
  
  # If the wkt strings are just points, a different rename method is needed
  if (class(sp_list[[1]])[1] == "SpatialPoints") {
    
    # Extract coordinates
    sp_list <- lapply(seq_along(sp_list), function(x) sp_list[[x]]@coords)
    
    # Bind all features
    sp <- sp_list_bind(sp_list)
    
    # All row names will be "1", this will case an error in the future, reset
    row.names(sp) <- NULL
    
    # Promote matrix to sp
    sp <- SpatialPoints(sp)
    
  } else {
    
    # Rename feature ids within list
    if (verbose) message("Binding geometries...")
    
    # Reset feature ids
    sp_list <- sp_reset_feature_ids(sp_list)

    # Bind all objects in list
    sp <- sp_list_bind(sp_list)
    
  }

  # Add data slots
  if (data & grepl("polygon", class(sp), ignore.case = TRUE)) {
    sp <- SpatialPolygonsDataFrame(sp, data = df_data, match.ID = FALSE)
  }
  
  if (data & grepl("lines", class(sp), ignore.case = TRUE)) {
    sp <- SpatialLinesDataFrame(sp, data = df_data, match.ID = FALSE)
  }
  
  if (data & grepl("points", class(sp), ignore.case = TRUE)) {
    sp <- SpatialPointsDataFrame(sp, data = df_data, match.ID = FALSE)
  }
  
  # Add wkt vector again
  if (data & keep) sp@data[, wkt] <- wkt_vector
  
  # Add projection
  if (!is.na(projection)) sp <- sp_transform(sp, projection, warn = FALSE)
  
  return(sp)
  
}
