#' Function to parse a data frame containing WKT strings to a spatial object. 
#' 
#' \code{sp_from_wkt} creates spatial objects from WKT strings and can create
#' spatial-data frames from the other variables contained within the data frame. 
#' \code{sp_from_wkt} is useful after querying a PostGIS database. 
#' 
#' @param df Data frame containing a WKT string variable. \code{df} can also be
#' a vector of WKT strings. 
#' @param wkt Variable name of WKT strings in \code{df}. 
#' @param data Should all variables other than \code{wkt} be added to the
#' spatial object's data-slot? I.e. create a spatial-data frame.
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Make a spatial object from many wkt strings
#' sp.from.wkt <- sp_from_wkt(data.wkt, wkt = "geom")
#' 
#' # Make a spatial-data frame object from many wkt strings
#' sp.data.from.wkt <- sp_from_wkt(data.wkt, wkt = "geom", data = TRUE)
#' 
#' }
#' 
#' @export
#'
sp_from_wkt <- function (df, wkt = "geom", data = FALSE, projection = NA) {
  
  # For vectors
  if (class(df) == "character") {
    
    # Vector is input
    wkt.vector <- df
    
    # Vectors will not contain a data slot
    data <- FALSE
    
  } else {
    
    # Catch dplyr's table
    df <- threadr::base_df(df)
    
    # Get a vector of wkt from df
    wkt.vector <- df[, wkt]
    
  }
  
  # Store data
  if (data) {
    
    df.data <- df
    df.data[, wkt] <- NULL
    
    # Overwrite row names
    row.names(df.data) <- seq_len(nrow(df.data))
    
  }
  
  # Parse WKT strings
  message("Parsing WKT strings...")
  sp.list <- pbapply::pblapply(wkt.vector, rgeos::readWKT)
  
  # If the wkt strings are just points, a different rename method is needed
  if (class(sp.list[[1]])[1] == "SpatialPoints") {
    
    # Extract coordinates
    sp.list <- lapply(seq_along(sp.list), function (x) sp.list[[x]]@coords)
    
    # Bind all features
    sp <- sp_list_bind(sp.list)
    
    # Promote to sp
    sp <- sp::SpatialPoints(sp)
    
    # Add row names, will be the same as data if matched later
    row.names(sp) <- as.character(1:length(sp))
    
  } else {
    
    # Rename feature ids within list
    message("Binding all spatial features together...")
    sp.list <- sp_rename(sp.list)
    
    # Bind all objects in list
    sp <- sp_list_bind(sp.list)
    
  }

  # Add data
  if (data & grepl("polygon", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialPolygonsDataFrame(sp, data = df.data)
  }
  
  if (data & grepl("lines", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialLinesDataFrame(sp, data = df.data)
  }
  
  if (data & grepl("points", class(sp), ignore.case = TRUE)) {
    sp <- sp::SpatialPointsDataFrame(sp, data = df.data)
  }
  
  # Add projection
  if (!is.na(projection)) {
    suppressMessages(
      sp <- sp_transform(sp, projection)
    )
  }
  
  # Return 
  sp
  
}


# Rename sp features within a list
sp_rename <- function (sp) {
  
  # Create an id vector
  id.vector <- seq_along(sp)
  id.vector <- as.character(id.vector)
  
  # Rename all elements in list
  sp <- lapply(seq_along(sp), function (x) sp::spChFIDs(sp[[x]], id.vector[x]))
  
  # Return
  sp
  
}


# Bind objects using do.call
sp_list_bind <- function (sp.list) {
  
  # Do call appends rather than create new
  sp <- do.call("rbind", sp.list)
  
  # Return
  sp
  
}


# Function for creating wkt strings from a spatial object. 
# To-do: add data handling too. 
# 
#' @rdname sp_from_wkt
#' @export
#' 
sp_to_wkt <- function (sp, features = TRUE) {
  string <- rgeos::writeWKT(sp, byid = features)
  string
}
