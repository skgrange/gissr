#' Function to print projections supported by R. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
print_projections <- function() rgdal::make_EPSG()


#' @rdname print_projections
#' @export
sp_all_projections <- print_projections