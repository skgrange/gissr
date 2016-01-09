#' Function to parse a data frame containing WKT strings to a spatial object. 
#' 
#' \code{sp_from_wkt} creates spatial objects from WKT strings and can create
#' spatial data frames from the other variables contained within the input data 
#' frame. \code{sp_from_wkt} is useful after querying a PostGIS database for 
#' geometries in WKT format. 
#' 
#' @param df Data frame containing a WKT string variable. \code{df} can also be
#' a vector of WKT strings. 
#' 
#' @param wkt Variable name of WKT strings in \code{df}. Default is \code{"geom"}.
#' 
#' @param data Should all variables other than \code{wkt} be added to the
#' spatial object's data-slot? I.e. create a spatial data frame.
#' 
#' @param projection A proj4 string to force the projection system after the WKT
#' strings have been parsed. Default is \code{NA}. 
#' 
#' @param verbose Should the function give messages? Useful when large number of
#' WKT strings are to be parsed. Default is \code{FALSE}. 
#' 
#' @param reset Should the feature IDs be reset before binding? Default is 
#' \code{TRUE} but may need to be altered for polygons. 
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
sp_from_wkt <- function (df, wkt = "geom", data = FALSE, projection = NA, 
                         verbose = FALSE, reset = TRUE) {
  
  # Catch dplyr's table data frame
  df <- threadr::base_df(df)
  
  # For vectors
  if (class(df) == "character") {
    # Vector is input
    wkt_vector <- df
    
    # Vectors will not contain a data slot
    data <- FALSE
    
  } else {
    # Get a vector of wkt from df
    wkt_vector <- df[, wkt]
    
  }
  
  # Store data
  if (data) {
    # Store
    df_data <- df
    df_data[, wkt] <- NULL
    
    # Overwrite row names
    row.names(df_data) <- NULL
    
  }
  
  # Parse WKT strings
  if (verbose) {
    message("Parsing WKT strings...")
    # Warning catch is for geoms with negative areas. Why does this occur? 
    suppressWarnings(
      sp_list <- pbapply::pblapply(wkt_vector, rgeos::readWKT)
    )
    
  } else {
    suppressWarnings(
      sp_list <- lapply(wkt_vector, rgeos::readWKT)
    )
    
  }
  
  # If the wkt strings are just points, a different rename method is needed
  if (class(sp_list[[1]])[1] == "SpatialPoints") {
    
    # Extract coordinates
    sp_list <- lapply(seq_along(sp_list), function (x) sp_list[[x]]@coords)
    
    # Bind all features
    sp <- sp_list_bind(sp_list)
    
    # Alter row names in matrix
    row.names(sp) <- NULL
    
    # Promote matrix to sp
    sp <- sp::SpatialPoints(sp)
    
    # Add row names, will be the same as data if matched later
    row.names(sp) <- as.character(1:length(sp))
    
  } else {
    
    # Rename feature ids within list
    if (verbose) {
      message("Binding all spatial features together...")
    }
    
    # Reset feature ids
    if (reset) {
      # sp_list <- sp_rename(sp_list)
      sp_list <- sp_reset_feature_ids(sp_list)
      
    }

    # Bind all objects in list
    sp <- sp_list_bind(sp_list)
    
  }

  # Add data slots
  if (data & grepl("polygon", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialPolygonsDataFrame(sp, data = df_data)
  }
  
  if (data & grepl("lines", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialLinesDataFrame(sp, data = df_data)
  }
  
  if (data & grepl("points", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialPointsDataFrame(sp, data = df_data)
  }
  
  # Add projection
  if (!is.na(projection)) {
    sp <- sp_transform(sp, projection, warn = FALSE)
  
  }
  
  # Return 
  sp
  
}


# Bind objects using do.call
sp_list_bind <- function (sp_list) {
  
  # Do call appends rather than create new
  sp <- do.call("rbind", sp_list)
  
  # Return
  sp
  
}


# Function for creating wkt strings from a spatial object. 
# To-do: add data handling too. 
# 
#' @rdname sp_from_wkt
#' @export
sp_to_wkt <- function (sp, features = TRUE) {
  string <- rgeos::writeWKT(sp, byid = features)
  string
}


# # Rename sp features within a list
# sp_rename <- function (sp) {
#   
#   # Create an id vector
#   id_vector <- seq_along(sp)
#   id_vector <- as.character(id_vector)
#   
#   # Rename all elements in list
#   sp <- lapply(seq_along(sp), function (x) sp::spChFIDs(sp[[x]], id_vector[x]))
#   
#   # Return
#   sp
#   
# }
