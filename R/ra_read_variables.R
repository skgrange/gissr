#' Function to read variables (and levels) in a raster data file. 
#' 
#' @param file Vector of file names containing raster data.
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
ra_read_variables <- function(file) {
  purrr::map_dfr(file, ra_read_variables_worker)
}


ra_read_variables_worker <- function(file) {
  
  # Probe file to get names, this captures the warning messages
  warning_messages <- raster_quiet(file)$warnings
  
  # Get variables from warning message
  variables <- warning_messages[1] %>% 
    stringr::str_split_fixed("of: ", 2) %>% 
    .[, 2] %>% 
    stringr::str_split(", ") %>% 
    .[[1]]
  
  # Get levels from warning message if it exists
  message_levels <- stringr::str_subset(warning_messages, "levels")
  
  if (length(message_levels) == 0L) {
    levels <- 1L
  } else {
    levels <- message_levels %>% 
      stringr::str_split_fixed("there are ", 2) %>% 
      .[ 2] %>% 
      stringr::str_split_fixed(" ", 2) %>% 
      .[, 1] %>% 
      as.integer()
  }
  
  # Make a tibble with all combinations
  df <- tidyr::expand_grid(
    file,
    level = seq_len(levels), 
    variable = variables
  )
  
  return(df)
  
}


raster_quiet <- purrr::quietly(raster::raster)
