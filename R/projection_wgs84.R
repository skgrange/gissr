#' Functions to wrap common projection (PROJ.4) strings. 
#' 
#' @author Stuart K.Grange
#' 
#' @export
projection_wgs84 <- function() "+proj=longlat +datum=WGS84 +no_defs"


#' @rdname projection_wgs84
#' @export
projection_google <- function() 
  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"


#' @rdname projection_wgs84
#' @export
projection_bng <- function() 
  "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"


#' @rdname projection_wgs84
#' @export
projection_nztm <- function() 
  "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
