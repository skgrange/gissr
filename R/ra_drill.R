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
  
  # Doubles to integers
  if (convert) df <- dplyr::mutate_if(df, is.numeric, type.convert, as.is = TRUE)
  
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
