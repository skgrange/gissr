#' Function to randomly sample n features in a spatial object. 
#' 
#' @param sp Spatial object.  
#' 
#' @param n Number of features to sample. 
#' 
#' @seealso \code{\link{set.seed}}
#'
#' @author Stuart K. Grange
#'
#' @export
sp_sample_n <- function(sp, n) sp[sample(nrow(sp), n), ]
