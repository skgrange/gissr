#' Function to simplify spatial objects using the Douglas-Peuker algorithm. 
#'
#' Holes will appear. 
#'
#' @param sp Spatial object which is to be simplified.
#' @param tolerance Tolerance value to be used by the Douglas-Peuker algorithm. 
#' @param preserve Should the algorithm attempt to preserve the topology? 
#' Default is TRUE.
#'
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' sp.simple <- sp_simplify(sp, 0.01)
#' }
#' 
#' @export
#'
sp_simplify <- function (sp, tolerance, preserve = TRUE) {
  
  # If a spatial data object is given, store the data piece of the object
  if (grepl("data", class(sp), ignore.case = TRUE)) {
    df <- data.frame(sp)
  }
  
  # Simplify spatial object with rgeos library
  sp <- rgeos::gSimplify(sp, tolerance, preserve)
  
  # Add the data if necessary as gSimplify drops this
  # To-do: ensure this exists function works only inside the function scope
  if (exists("df")) {
    sp <- sp::SpatialPolygonsDataFrame(sp, df)
  }
  
  # Return
  sp
  
}
