#' Functions to convert points among different projection systems. 
#' 
#' These functions use spatial libraries to do the coordinate conversion so the
#' sometimes many-step calculations do not have to be maintained. 
#' 
#' @author Stuart K. Grange
#'
#' @export
wgs84_to_osgb36 <- function(y, x) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to wgs84
  sp <- sp_transform(sp, warn = FALSE)
  
  # Do the conversion to osgb36
  sp_transformed <- sp_transform(sp, "bng")
  
  # Extract coordinates from spatial object
  coordinates <- sp_transformed@coords
  coordinates <- data.frame(x = coordinates[, 1], y = coordinates[, 2])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}


#' @rdname wgs84_to_osgb36
#' @export
osgb36_to_wgs84 <- function(x, y) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to osgb36
  sp <- sp_transform(sp, "bng", warn = FALSE)
  
  # Do the conversion to wgs84
  sp_transformed <- sp_transform(sp)
  
  # Extract coordinates from spatial object
  coordinates <- sp_transformed@coords
  
  # Order is different for lat and longs
  coordinates <- data.frame(latitude = coordinates[, 2], longitude = coordinates[, 1])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}


# From rnrfa package, https://github.com/cvitolo/r_rnrfa
# 
# No export
#
OSGParse <- function(gridRef) {
  
  if ( length(gridRef) > 1 ) {
    
    x <- unlist(gridRef)
    df <- data.frame(matrix(NA,ncol=2,nrow=length(x))) 
    names(df) <- c("Easting","Northing")
    
    for (i in 1:length(x)){
      df[i,"Easting"] <- OSGParse1(x[i])[1]
      df[i,"Northing"] <- OSGParse1(x[i])[2]
    }    
    
  }else{
    df <- data.frame(matrix(NA,ncol=2,nrow=1))
    names(df) <- c("Easting","Northing")
    df[1,"Easting"] <- OSGParse1(gridRef)[1]
    df[1,"Northing"] <- OSGParse1(gridRef)[2]
  }
  
  return(df)
  
}

# From rnrfa package, https://github.com/cvitolo/r_rnrfa
# 
# No export
#
OSGParse1 <- function(gridRef) {
  
  gridRef <- toupper(gridRef)
  
  # get numeric values of letter references, mapping A->0, B->1, C->2, etc:
  l1 <- as.numeric(charToRaw(substr(gridRef,1,1))) - 65
  l2 <- as.numeric(charToRaw(substr(gridRef,2,2))) - 65
  
  # shuffle down letters after 'I' since 'I' is not used in grid:
  if (l1 > 7) l1 <- l1 - 1
  if (l2 > 7) l2 <- l2 - 1
  
  # convert grid letters into 100km-square indexes from false origin - grid square SV
  
  e <- ((l1-2) %% 5) * 5 + (l2 %% 5)
  n <- (19 - floor(l1/5) *5 ) - floor(l2/5)
  
  if (e<0 || e>6 || n<0 || n>12) { return(c(NA,NA)) }
  
  # skip grid letters to get numeric part of ref, stripping any spaces:
  
  ref.num <- gsub(" ", "", substr(gridRef, 3, nchar(gridRef)))
  ref.mid <- floor(nchar(ref.num) / 2)
  ref.len <- nchar(ref.num)
  
  if (ref.len >= 10) { return(c(NA,NA)) }
  
  e <- paste(e, substr(ref.num, 0, ref.mid), sep="", collapse="")
  n <- paste(n, substr(ref.num, ref.mid+1, ref.len), sep="", collapse="")
  
  nrep <- 5 - match(ref.len, c(0,2,4,6,8))
  
  e <- as.numeric(paste(e, "0", rep("0", nrep), sep="", collapse=""))
  n <- as.numeric(paste(n, "0", rep("0", nrep), sep="", collapse=""))
  
  return(c(e,n))
  
}


#' @rdname wgs84_to_osgb36
#' @export
os_grid_to_osgb36 <- function(grid_reference) {
  
  # Use other function
  df <- OSGParse(grid_reference)
  
  # Rename
  df <- plyr::rename(df, c("Easting" = "x", "Northing" = "y"))
  
  # Return
  df
  
}


#' @rdname wgs84_to_osgb36
#' @export
nztm_to_wgs84 <- function(x, y) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to nztm
  sp <- sp_transform(sp, "nztm", warn = FALSE)
  
  # Do the conversion to wgs84
  sp_transformed <- sp_transform(sp)
  
  # Extract coordinates from spatial object
  coordinates <- sp_transformed@coords
  
  # Order is different for lat and longs
  coordinates <- data.frame(latitude = coordinates[, 2], longitude = coordinates[, 1])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}


#' @rdname wgs84_to_osgb36
#' @export
wgs84_to_nztm <- function(y, x) {
  
  # Make a matrix
  # Order: x, y
  point <- cbind(x, y)
  
  # Spatial points
  sp <- SpatialPoints(point)
  
  # Force sp projection to wgs84
  sp <- sp_transform(sp, warn = FALSE)
  
  # Do the conversion to osgb36
  sp_transformed <- sp_transform(sp, "nztm")
  
  # Extract coordinates from spatial object
  coordinates <- sp_transformed@coords
  coordinates <- data.frame(x = coordinates[, 1], y = coordinates[, 2])
  row.names(coordinates) <- NULL
  
  # Return
  coordinates
  
}
