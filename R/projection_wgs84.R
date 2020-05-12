#' Functions to wrap common projection (PROJ.4) strings. 
#' 
#' @author Stuart K.Grange
#' 
#' @return Character vector with length of 1.
#' 
#' @seealso \href{http://spatialreference.org/}{spatialreference.org}
#' 
#' @export
projection_wgs84 <- function() {
  "+proj=longlat +datum=WGS84 +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_google <- function() {
  "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_bng <- function() {
  "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_nztm <- function() {
  "+proj=tmerc +lat_0=0 +lon_0=173 +k=0.9996 +x_0=1600000 +y_0=10000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_us_albers <- function() {
  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_antarctic <- function() {
  "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_norway <- function() {
  "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=18 +k=0.994 +x_0=2000000 +y_0=2000000 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_lcc <- function() {
  "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_swiss <- function() {
  "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_swiss_plus <- function() {
  "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_slovenia <- function() {
  "+proj=tmerc +lat_0=0 +lon_0=15 +k=0.9999 +x_0=500000 +y_0=-5000000 +ellps=bessel +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_utm_32 <- function() {
  "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_utm_33 <- function() {
  "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_madrid <- function() {
  "+proj=lcc +lat_1=40 +lat_0=40 +lon_0=0 +k_0=0.9988085293 +x_0=600000 +y_0=600000 +a=6378298.3 +b=6356657.142669561 +pm=madrid +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_mollweide <- function() {
  "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_china <- function() {
  "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_beijing <- function() {
  "+proj=lcc +lat_1=20 +lat_2=50 +lat_0=39.51775 +lon_0=110 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_etr89 <- function() {
  "+proj=laea +lat_0=50 +lon_0=11 +x_0=5000000 +y_0=3200000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
}


#' @rdname projection_wgs84
#' @export
projection_italy <- function() {
  "+proj=tmerc +lat_0=0 +lon_0=9 +k=0.9996 +x_0=1500000 +y_0=0 +ellps=intl +units=m +no_defs"
}
