#' Function to move/shift a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' 
#' @param x Offset amount (in \code{sp} projection's units) in left-right 
#' dimension.
#' 
#' @param y Offset amount (in \code{sp} projection's units) in up-down 
#' dimension.
#' 
#' @seealso \code{\link{elide}}
#' 
#' @return Spatial object.  
#' 
#' @export
sp_move <- function(sp, x, y) {
  
  # Get projection string
  projection <- sp_projection(sp)
  
  # Do
  sp <- sp::elide(sp, shift = c(x, y))   
  
  # Add projection again
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  return(sp)
  
}


# Wrap the function
#' @rdname sp_move
#' @export
sp_shift <- sp_move
