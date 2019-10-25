#' Function to filter a spatial object by another spatial object 
#' (usually a spatial polygon). 
#' 
#' Both spatial objects must have identical projection systems. 
#' 
#' @param sp Spatial object to be filtered. 
#' 
#' @param sp_boundary Usually a spatial polygon to be used as the filtering 
#' element for \code{sp}. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Filtered version of \code{sp}.
#' 
#' @seealso \code{\link{sp_crop}}, \code{\link{sp_intersect}}
#'
#' @export
sp_filter <- function(sp, sp_boundary) {
  
  # Check the projection systems
  if (!identical(sp_projection(sp), sp_projection(sp_boundary))) {
    stop("Projection systems are not identical...", call. = FALSE)
  }

  # Filter
  sp <- sp[sp_boundary, ]
  
  return(sp)
  
}
