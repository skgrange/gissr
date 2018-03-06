#' Function to bind different spatial objects together. 
#' 
#' \code{sp_bind} combines spatial objects of the same type together. This 
#' function is useful for combining shapefiles or tables of WKT strings
#' together. The feature IDs within line- and polygon-spatial objects must be 
#' manipulated for the binding to occur. The original, non-manipulated IDs are 
#' not preserved. \code{sp_bind} will still bind objects if their data slots
#' contain different variables. 
#' 
#' For uniting geometries which exist in the same data object, use 
#' \code{\link{sp_unite}}. 
#' 
#' @param ... Spatial objects of the same type. \code{...} can be a number of 
#' individual objects or a list containing many objects. 
#'
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{sp_unite}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Two objects
#' sp_london_two <- sp_bind(sp_croydon, sp_islington)
#' 
#' # Many objects
#' sp_london_many <- sp_bind(sp_croydon, sp_islington, sp_bexley, sp_brent)
#' 
#' # Or a list of many objects
#' sp_london_many_many <- sp_bind(sp_list_of_many)
#' 
#' 
#' # Load many files with plyr then bind together
#' # Get files
#' file_list <- list.files("data/shapefiles", "shp")
#' 
#' # Load files
#' sp_list <- llply(file_list, sp_read, verbose = FALSE)
#' 
#' # Bind all spatial objects
#' sp_bound <- sp_bind(sp_list)
#' 
#' }
#'
#' @export
sp_bind <- function(...) {
  
  # Get input
  list_sp <- list(...)
  
  # Drop null objects in list
  list_sp <- purrr::discard(list_sp, ~ length(.x) == 0 | is.null(.x))
  
  # When input is already a list the ... makes a list of a list
  if (length(list_sp) == 1 & sapply(list_sp[1], class) == "list")
    list_sp <- unlist(list_sp)
  
  # Class check
  if (length(unique(sapply(list_sp, class))) != 1)
    stop("Geometries must be of the same type to be bound.", call. = FALSE)
  
  # Bind
  sp_bind <- sp_bind_many(list_sp)

  return(sp_bind)
  
}


# No export
sp_bind_many <- function(sp_list) {
  
  # Class check
  if (!is.list(sp_list)) stop("Input must be list.", .call = FALSE)
  
  # Points can be easily bound with do.call
  if (grepl("point", class(sp_list[[1]]), ignore.case = TRUE)) {
    
    sp_bind <- sp_list_bind(sp_list)
    
  } else {
    
    # Reset ids, this will use uuids for uniqueness
    sp_list <- sp_reset_feature_ids(sp_list)
    
    # Bind
    sp_bind <- sp_list_bind(sp_list)
    
    # Reset ids again to something suitable, a character vector
    sp_bind <- sp_reset_feature_ids(sp_bind)
  
  }
  
  return(sp_bind)
  
}


sp_list_bind <- function(sp_list) {
  
  sp <- tryCatch({
    
    # Bind objects using do.call
    do.call("rbind", sp_list)
    
  }, error = function(e) {
    
    # On error, try some fancy data slot manipulations
    # Get names of all variables in data slot
    names <- lapply(sp_list, function(x) names(x@data))
    
    # Make a name vector
    names <- unlist(names)
    names <- unique(names)
    
    # Create data frame with zero rows
    names <- stringr::str_c(names, collapse = ",")
    df <- read.csv(textConnection(names), stringsAsFactors = FALSE)
    
    # Create new data slots with the same variable names
    # sp_data_slot <- lapply(sp_list, function(x) plyr::rbind.fill(x@data, df))
    
    # A for loop in R!? 
    for (x in 1:length(sp_list)) {
      
      # Row names need to persist
      row_names <- rownames(sp_list[[x]]@data)
      
      # Alter data slot
      sp_list[[x]]@data <- plyr::rbind.fill(sp_list[[x]]@data, df)
      
      # Add row names again
      rownames(sp_list[[x]]@data) <- row_names
      
    }
    
    # Return
    do.call("rbind", sp_list)
    
  })
  
  return(sp)

}
