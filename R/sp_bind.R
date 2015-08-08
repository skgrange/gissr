#' Functions to bind/combine spatial objects together. 
#' 
#' \code{sp_bind} combines two similar spatial objects together with the same 
#' syntax of \code{rbind}. Currently, only two objects can be bound together. 
#' \code{sp_bind_many} combines multiple spatial objects together but 
#' the input must be a list. These function are useful for combining shape files
#' together. 
#' 
#' The feature IDs within the spatial objects must be manipulated for the 
#' binding to occur. The original, non-manipulated IDs are not preserved. 
#' 
#' @param sp Spatial object one. 
#' @param sp.2 Spatial object two.
#' @param sp.list A list containing spatial two or more spatial objects. 
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Two objects
#' sp.london.two <- sp_bind(sp.croydon, sp.islington)
#' 
#' # Many objects, note the nested list
#' sp.london.many <- sp_bind_many(list(sp.croydon, sp.islington, sp.bexley, 
#'   sp.brent))
#' }
#'
#' @export
#'
sp_bind <- function (sp, sp.2) {
  
  # Get length of first spatial object
  n <- length(sp)
  
  # Create a sequence of ids
  id <- 1:n
  id.character <- as.character(id)
  
  # Alter feature ids within sp
  sp <- sp::spChFIDs(sp, id.character)
  
  # Store final id for next object manipulation
  id.push <- id[length(id)]
  # Next object has the next feature
  id.push <- id.push + 1
  
  # Length of second object
  n.2 <- length(sp.2)
  id.2 <- seq(id.push, length.out = n.2)
  id.2.character <- as.character(id.2)
  
  # Alter feature ids within sp
  sp.2 <- sp::spChFIDs(sp.2, id.2.character)
  
  # Bind/combine objects
  sp.combine <- maptools::spRbind(sp, sp.2)
  
  # Return
  sp.combine
  
}


#' @rdname sp_bind
#' 
#' @export
#'
sp_bind_many <- function (sp.list, progress = TRUE) {
  
  if (class(sp.list) != "list") {
    stop("The input must be list of spatial objects")
  }
  
  # A for loop!?
  # Set-up progress bar
  if (progress) {
    pb <- txtProgressBar(min = 0, max = length(sp.list), style = 3)
  }
  
  for (i in seq_along(sp.list)) {
    
    if (i == 1) {
      
      # The first loop, just bind the first two objects
      sp.bind <- sp_bind(sp.list[[1]], sp.list[[2]])
      
    } else {
      
      # Accumulate sp.bind and add the extra objects
      sp.bind <- sp_bind(sp.bind, sp.list[[i]])
      
    }
    
    # Update progress bar
    if (progress) {
      setTxtProgressBar(pb, i)
    }
    
  }
  
  # Return
  sp.bind
  
}
