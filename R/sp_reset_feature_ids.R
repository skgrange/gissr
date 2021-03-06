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
#' @author Stuart K. Grange
#' 
#' @return Spatial object. 
#' 
#' @examples 
#' \dontrun{
#' 
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
#' @export
sp_reset_feature_ids <- function(sp) {
  
  # A list of spatial objects? 
  if (class(sp) == "list") {
    
    # Will use uuids for the features to ensure uniqueness
    sp <- lapply(sp, function(x) resetter(x, uuid = TRUE))
    
    # Reset row names too, not in lapply
    sp <- reset_data_slot(sp)
    
  } else {
    
    # Reset spatial features
    sp <- resetter(sp)
    
    # Reset row names too
    sp <- reset_data_slot(sp)
    
  }
  
  return(sp)
  
}


# Reset feature ids
resetter <- function(sp, uuid = FALSE) {
  
  if (uuid) {
    # Use uuids to ensure unique-ness 
    sp <- spChFIDs(sp, replicate(length(sp), threadr::uuid()))
  } else {
    # Otherwise, just a character sequence
    sp <- spChFIDs(sp, as.character(seq_along(sp)))
  }
  
  return(sp)
  
}


# For resetting data slots
reset_data_slot <- function(sp) {
  
  # Only if object has a data slot
  if (grepl("data", sp_class(sp), ignore.case = TRUE)) row.names(sp@data) <- NULL
  
  return(sp)
  
}
