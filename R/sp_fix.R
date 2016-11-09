#' Function to fix polygons when they have issues with validity and holes. 
#'
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. Polygons are currently the only object-type which
#' are supported. 
#' 
#' @param verbose Should the function print messages? Default is \code{TRUE}. 
#'
#' @examples 
#' \dontrun{
#' sp_clean <- sp_fix(sp_pm10)
#' }
#' 
#' @export
sp_fix <- function(sp, verbose = TRUE) 
  cleangeo::clgeo_Clean(sp, verbose = verbose)
