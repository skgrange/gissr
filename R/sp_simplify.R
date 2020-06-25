#' Function to simplify spatial objects using the Douglas-Peuker algorithm. 
#'
#' Holes will appear between the geometries at times. 
#'
#' @param sp Spatial object which is to be simplified.
#' 
#' @param tolerance Tolerance value to be used by the Douglas-Peuker algorithm. 
#' 
#' @param preserve Should the algorithm attempt to preserve the topology? 
#' Default is \code{TRUE}.
#'
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Simplify an object
#' sp_simple <- sp_simplify(sp, 0.01)
#' 
#' }
#' 
#' @export
sp_simplify <- function(sp, tolerance, preserve = TRUE) {
  rgeos::gSimplify(sp, tolerance, preserve)
}
