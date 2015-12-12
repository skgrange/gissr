#' Function to reset spatial object's feature IDs. 
#' 
#' \code{sp_reset_feature_ids} will reset the feature IDs in a spatial object to
#' sequential integers (beginning at 1), but these will be stored as a characters.
#' If the spatial object has a data slot, the row names of the data frame will 
#' also be reset.
#' 
#' @param sp A spatial object. \code{sp} can also be a list containing many 
#' spatial objects. 
#' 
#' @examples 
#' \dontrun{
#' # Load data
#' sp_zones <- sp_read("zones.json")
#' 
#' # Filter
#' sp_zones <- subset(sp_zones, country == "DE")
#' 
#' # Reset feature ids
#' sp_zones <- sp_reset_feature_ids(sp_zones)
#' 
#' }
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_reset_feature_ids <- function (sp) {
  
  # A list of spatial objects? 
  if (class(sp) == "list") {
    # Create an id vector
    id_vector <- as.character(seq_along(sp))
    
    # Rename all elements within list
    # To-do, enhance the resetter to include a id argument
    sp <- lapply(seq_along(sp), function (x) sp::spChFIDs(sp[[x]], id_vector[x]))
    
    # Reset row names too
    sp <- lapply(sp, reset_data_slot)
    
  } else {
    # Reset spatial features
    sp <- resetter(sp)
    
    # Reset row names too
    sp <- reset_data_slot(sp)
    
  }
  
  # Return
  sp
  
}

# Reset feature ids
resetter <- function (sp) sp::spChFIDs(sp, as.character(seq_along(sp)))

# For resetting data slots
reset_data_slot <- function (sp) {
  if (grepl("data", class(sp), ignore.case = TRUE)) {
    row.names(sp@data) <- NULL
  }
  sp
}
