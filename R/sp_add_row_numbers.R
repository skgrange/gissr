#' Function to add a row number variable to a spatial object's data slot. 
#' 
#' @param sp Spatial object containing a data slot. 
#' 
#' @param name Name of variable to use.
#' 
#' @param zero_based Should the numbering start at 0 rather than 1? 
#' 
#' @author Stuart K. Grange
#' 
#' @export
sp_add_row_numbers <- function(sp, name = "id", zero_based = FALSE) {
  
  # Check
  if (!grepl("data", sp_class(sp), ignore.case = TRUE)) {
    stop("Spatial object must have a data slot.", call. = FALSE) 
  }
  
  # Add id to data slot
  sp@data <- tibble::rowid_to_column(sp@data, var = name)
  
  if (zero_based) {
    # Get id
    id_sequence <- pull(sp@data, !!name)
    id_sequence <- id_sequence - 1L
    # Overwrite old sequence
    sp@data <- mutate(sp@data, {{name}} := !!id_sequence)
  }
  
  return(sp)
  
}
