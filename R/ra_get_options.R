#' Function to print the \strong{raster} package's options. 
#' 
#' @param x Function specific option. 
#' 
#' @export
ra_get_options <- function() {
  x <- threadr::quiet(raster::rasterOptions())
  as_tibble(x)
}


#' @rdname ra_get_options
#' @export
ra_set_temporary_directory <- function(x) {
  threadr::quiet(raster::rasterOptions(tmpdir = x))
}
