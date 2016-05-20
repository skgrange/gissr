#' Function to randomly sample n features in a spatial object. 
#'
#' @author Stuart K. Grange
#'
#' @export
sp_sample_n <- function(sp, n) sp[sample(nrow(sp), n), ]
