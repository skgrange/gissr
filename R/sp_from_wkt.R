#' Function to parse a data frame containing WKT strings to a spatial object. 
#' 
#' \code{sp_from_wkt} creates spatial objects from WKT strings and can create
#' spatial-data frames from the other variables contained within the data frame. 
#' 
#' @param df Data frame containing a WKT string variable. \code{df} can also be
#' a vecotr of WKT strings. 
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
sp_from_wkt <- function (df, wkt = "geom", data = FALSE) {
  
  # Catch vector
  if (class(df) == "character") {
    
    # Vector is input
    wkt.vector <- df
    
    # Vectors will not contain a data slot
    data <- FALSE
    
  } else {
    
    # Get a vector of wkt from df
    wkt.vector <- df[, wkt]
    
  }
  
  # Store data
  if (data) {
    
    df.data <- df
    df.data[, wkt] <- NULL
    
  }
  
  # Parse WKT strings
  message("Parsing WKT strings...")
  sp.list <- lapply(wkt.vector, rgeos::readWKT)
  
  # If the wkt strings are just points, a different method is needed
  if (class(sp.list[[1]])[1] == "SpatialPoints") {
    
    message("Extracting coordinates from spatial points...")
    sp.list <- lapply(seq_along(sp.list), function (x) 
      extract_point_coordinates(sp.list, x))
    
    # Bind all features
    sp <- sp_list_bind(sp.list)
    
    # Promote to sp
    sp <- sp::SpatialPoints(sp)
    
  } else {
    
    # Rename feature ids within list
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
  
  # Return 
  sp
  
}


sp_rename <- function (sp) {
  
  # Create an id vector
  id.vector <- seq_along(sp)
  id.vector <- as.character(id.vector)
  
  # Rename all elements in list
  sp <- lapply(seq_along(sp), function (x) 
    sp::spChFIDs(sp[[x]], id.vector[x]))
  
  # Return
  sp
  
}


sp_list_bind <- function (sp.list) {
  sp <- do.call("rbind", sp.list)
  sp
}


extract_point_coordinates <- function (sp.list, index) {
  
  # Extract coordinates from slot
  coordinates <- sp.list[[index]]@coords
  
  # Build a data frame, watch the order
  df <- data.frame(x = coordinates[2], y = coordinates[1])
  
  # Return
  df
  
}
