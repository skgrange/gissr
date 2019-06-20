#' Function to read all geoms in a spatial data file. 
#' 
#' @param file Spatial data file to read. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list containing spatial objects of different data types.
#' 
#' @seealso \code{\link{readOGR}}, \code{\link{sp_list_drivers}}, 
#' \code{\link{sp_list_layers}}, \code{\link{sp_read}}
#' 
#' @export
sp_read_all_geoms <- function(file, verbose = FALSE) {
  
  # Check input
  stopifnot(file.exists(file))
  
  if (verbose) message(threadr::date_message(), "Reading points...")
  
  suppressMessages(
    sp_points <- sp_read_safe(file, geom = "points", verbose = FALSE)
  )
  
  if (verbose) message(threadr::date_message(), length(sp_points), " points...")
  
  if (verbose) message(threadr::date_message(), "Reading lines...")
  
  suppressMessages(
    sp_lines <- sp_read_safe(file, geom = "lines", verbose = FALSE)
  )
  
  if (verbose) message(threadr::date_message(), length(sp_lines), " lines...")
  
  if (verbose) message(threadr::date_message(), "Reading polygons...")
  
  suppressMessages(
    sp_polygons <- sp_read_safe(file, geom = "polygons", verbose = FALSE)
  )
  
  if (verbose) message(threadr::date_message(), length(sp_polygons), " polygons...")
  
  if (verbose) message(threadr::date_message(), "Binding spatial objects together...")
  
  # Bind together
  list_sp <- list(
    points = sp_points,
    lines = sp_lines,
    polygons = sp_polygons
  )
  
  return(list_sp)
  
}


# Keep things safe
sp_read_safe <- purrr::possibly(sp_read, otherwise = NULL)
