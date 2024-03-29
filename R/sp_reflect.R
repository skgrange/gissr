#' Function to reflect/reverse a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param x Should \code{sp} be reflected in the \code{x} dimension? 
#' 
#' @param y Should \code{sp} be reflected in the \code{y} dimension? 
#' 
#' @seealso \code{\link{elide}}
#' 
#' @export
sp_reflect <- function(sp, x = FALSE, y = FALSE) {
  
  # Get projection string
  projection <- sp_projection(sp)
  
  # Do
  sp <- sp::elide(sp, reflect = c(x, y))
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  return(sp)
  
}
