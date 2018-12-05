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
#' @param fun Function to summarise the values. 
#' 
#' @param na.rm Should \code{NA}s be ommited when using \code{fun}?
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{extract}}, \code{\link{tidy_ra_drill}}
#' 
#' @return Tibble. 
#' 
#' @export
ra_drill <- function(ra, sp, method = "simple", fun = mean, na.rm = TRUE) {
  
  # Checks
  stopifnot(is.ra(ra))
  stopifnot(is.sp(sp))
  
  if (!sp_projection(ra) == sp_projection(sp)) {
    
    stop(
      "Projection systems of the raster and spatil objects are not identical", 
      call. = FALSE
    )
    
  }
  
  # Extract values from the raster object
  df <- raster::extract(
    x = ra,
    y = sp, 
    method = method, 
    fun = fun, 
    na.rm = na.rm, 
    cellnumbers = TRUE,
    df = TRUE
  ) %>% 
    as_tibble()
  
  # Clean names
  names(df) <- ifelse(names(df) == "ID", "id_sp", names(df))
  names(df) <- ifelse(names(df) == "cells", "cell_raster", names(df))
  
  # Doubles to integers
  df <- dplyr::mutate_if(df, is.numeric, type.convert, as.is = TRUE)
  
  return(df)
  
}


#' Function to reshape the return from \code{\link{ra_drill}} to be "tidy-data".
#' 
#' @param df Data frame/tibble from \code{\link{ra_drill}}
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @seealso \code{\link{ra_drill}}
#' 
#' @export
tidy_ra_drill <- function(df) {
  
  # What variables should be used as keys?
  # Depends on spatial data type
  if ("cell_raster" %in% names(df)) {
    
    variable_keys <- c("id_sp", "cell_raster")
    
  } else {
    
    variable_keys <- "id_sp"
    
  }
  
  # Make tidy data
  df <- tidyr::gather(df, variable, value, -!!variable_keys) 
  
  return(df)
  
}
