#' Function to test weather one spatial object intersects with another. 
#' 
#' @param sp_one First spatial object to be tested. 
#' 
#' @param sp_two Second spatial object to be tested.  
#' 
#' @param features Should the calculation be calculated on every feature of 
#' \code{sp_one} and \code{sp_two}. 
#' 
#' @param simplify Should the return be simplified to a logical vector? 
#' 
#' @param check Should the intersection function perform some checks? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Logical matrix or vector. 
#' 
#' @seealso \code{\link{sp_filter}}, \code{\link{sp_crop}}
#' 
#' @export
sp_intersect <- function(sp_one, sp_two, features = TRUE, simplify = FALSE, 
                         check = FALSE) {
  
  # Check projections
  stopifnot(identical(sp_projection(sp_one), sp_projection(sp_two)))
  
  # Do the calculation, returns a matrix
  suppressWarnings(
    x <- rgeos::gIntersects(
      sp_one, 
      sp_two, 
      byid = features, 
      prepared = TRUE, 
      returnDense = TRUE, 
      checkValidity = check
    )
  )
  
  # Make a logical vector if desired
  if (simplify) {
    x <- unname(apply(x, 1, any))
  }
  
  return(x)
  
}
