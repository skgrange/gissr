#' Function to convert degrees, minutes, and seconds (DMS) to decimal degrees. 
#' 
#' @param degrees Degrees vector. 
#' @param minutes Minutes vector. 
#' @param seconds Seconds vector. 
#' @param convert Force the input to be numeric vectors. Default is \code{FALSE}. 
#' 
#' @return Numeric vector representing \code{degrees}, \code{minutes}, and
#' \code{seconds} as decimal degrees. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
dms_to_decimal <- function(degrees, minutes, seconds, convert = FALSE) {
  
  # Ensure that vectors are numeric, will work with factors
  if (convert) {
    
    degrees <- as.numeric(as.character(degrees))
    minutes <- as.numeric(as.character(minutes))
    seconds <- as.numeric(as.character(seconds))
    
  }
  
  # The conversion
  decimal <- degrees + minutes / 60 + seconds / 3600
  
  # Return
  decimal
  
}
