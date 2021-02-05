#' Function to `drill' into a raster object and return values. 
#' 
#' \code{ra_drill} wraps the \code{\link{extract}} raster function. 
#' 
#' @param ra Raster object. 
#' 
#' @param sp Spatial object to drill into ra to get values for. 
#' 
#' @param method Method for extraction, \code{"simple"} or \code{"bilinear"} for
#' interpolation. 
#' 
#' @param convert Should \code{type.convert} be used on the return? 
#' 
#' @param fun Function to summarise the values. 
#' 
#' @param na.rm Should \code{NA}s be omitted when using \code{fun}?
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link[raster]{extract}}, \code{\link{tidy_ra_drill}}, 
#' \code{\link{ra_drill_with_cell_number}}
#' 
#' @return Tibble. 
#' 
#' @export
ra_drill <- function(ra, sp, method = "simple", convert = FALSE, fun = mean, 
                     na.rm = TRUE) {
  
  # Checks
  stopifnot(is.ra(ra) & is.sp(sp))
  
  if (!sp_projection(ra) == sp_projection(sp)) {
    stop(
      "Projection systems of the raster and spatial objects are not identical.", 
      call. = FALSE
    )
  }
  
  # Extract values from the raster object
  if (stringr::str_detect(sp_class(sp), "Point")) {
    
    df <- raster::extract(
      x = ra,
      y = sp, 
      method = method, 
      fun = fun, 
      na.rm = na.rm, 
      cellnumbers = TRUE,
      df = TRUE
    ) %>% 
      as_tibble() %>% 
      rename(id_sp = ID,
             cell_raster = cells)
    
  } else if (stringr::str_detect(sp_class(sp), "Line")) {
    
    df <- raster::extract(
      x = ra,
      y = sp, 
      method = method, 
      fun = NULL, 
      na.rm = na.rm, 
      cellnumbers = TRUE,
      df = FALSE
    ) %>% 
      purrr::map_dfr(as_tibble, .id = "id_sp") %>% 
      mutate(id_sp = as.integer(id_sp)) %>% 
      rename(cell_raster = cell)
    
  } 
  
  # Doubles to integers
  if (convert) df <- dplyr::mutate_if(df, is.numeric, type.convert, as.is = TRUE)
  
  return(df)
  
}


#' Function to reshape the return from \code{\link{ra_drill}} to be "tidy-data".
#' 
#' @param df Data frame/tibble from \code{\link{ra_drill}}.
#' 
#' @param variable_as_date Are the variables/names of the \code{\link{ra_drill}}
#' output dates? If so, the dates will be parsed and the variable renamed. 
#' 
#' @param tz If \code{variable_as_date} is \code{TRUE}, what time zone are the 
#' dates stored in? This will almost certainly be \code{UTC}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{ra_drill}}
#' 
#' @export
tidy_ra_drill <- function(df, variable_as_date = FALSE, tz = "UTC") {
  
  # What variables should be used as keys?
  # Depends on spatial data type
  if ("cell_raster" %in% names(df)) {
    variable_keys <- c("id_sp", "cell_raster")
  } else {
    variable_keys <- "id_sp"
  }
  
  # Make longer data
  df <- tidyr::pivot_longer(df, -!!variable_keys, names_to = "variable")
  
  # If the names are dates, rename variable and parse
  if (variable_as_date && stringr::str_detect(df$variable[1], "^X")) {
    df <- df %>% 
      rename(date = variable) %>% 
      mutate(date = stringr::str_remove(date, "^X"),
             date = lubridate::ymd_hms(date, tz = tz, truncated = 3))
  }
  
  return(df)
  
}
