#' Function to fix polygons when they have issues with validity and holes. 
#'
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. Polygons are currently the only object-type which
#' are supported. 
#' 
#' @param print Should the features which are corrected, and why be displayed to
#' the user? Default is \code{TRUE}. 
#'
#' @examples 
#' \dontrun{
#' sp_clean <- sp_fix(sp_pm10)
#' }
#' 
#' @export
sp_fix <- function(sp, print = TRUE) cleangeo::clgeo_Clean(sp, print.log = print)
