#' Function to extract bounding box from a spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sp Spatial object. 
#' @param vector Should the bounding box be returned as a vector with the order
#' xmin, xmax, ymin, ymax? Default is \code{TRUE}. 
#' 
#' @export
sp_bounding_box <- function(sp, vector = TRUE) {
  
  # Get bounding box
  box <- sp@bbox
  
  if (vector) {
    
    # Make a vector, xmin, xmax, ymin, ymax
    box <-  c(box[1, 1], box[1, 2], box[2, 1], box[2, 2])
    
  }
  
  # Return
  box
  
}
