#' Functions to bind/combine spatial objects together. 
#' 
#' \code{sp_bind} combines two similar spatial objects together with the same 
#' syntax of \code{rbind}. Currently, only two objects can be bound together. 
#' \code{sp_bind_many} combines multiple spatial objects together but 
#' the input must be a list. 
#' 
#' These function are useful for combining shape files or tables of WKT strings
#' together. 
#' 
#' The feature IDs within line- and polygon-spatial objects must be manipulated 
#' for the binding to occur. The original, non-manipulated IDs are not 
#' preserved. 
#' 
#' To-do: This function accumulates a spatial object in a for-loop which is 
#' inefficient. The binding process should be handled by \code{do.call("rbind")}. 
#' This needs to be solved soon. 
#' 
#' @param sp Spatial object one. 
#' 
#' @param sp_2 Spatial object two.
#' 
#' @param force Should the feature IDs be forced to change? This may be 
#' necessary when feature IDs are not sequential.
#' 
#' @param sp_list A list containing spatial two or more spatial objects. 
#'
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' # Two objects
#' sp_london_two <- sp_bind(sp_croydon, sp_islington)
#' 
#' # Many objects, note the nested list
#' sp_london_many <- sp_bind_many(list(sp_croydon, sp_islington, sp_bexley, 
#'   sp_brent))
#' }
#'
#' @export
sp_bind <- function (sp, sp_2) {
  
  # Class check
  if (!class(sp) == class(sp_2)) {
    stop("Spatial objects must be of the same type to be bound.")
  }
  
  # Points can be easily bound with do.call
  if (grepl("point", class(sp), ignore.case = TRUE)) {
    # rbind
    sp_combine <- do.call("rbind", list(sp, sp_2))
    
    # For lines and polygons
  } else {
    # Change ids, this is wasteful but robust
    # I have used logic to handle this, but at times the binding fails due to non-
    # sequential ids
    # First object
    sp <- sp::spChFIDs(sp, as.character(1:length(sp)))
    
    # Second object
    sp_2 <- sp::spChFIDs(sp_2, as.character(length(sp) + 1:length(sp_2)))
    
    # Bind objects
    sp_combine <- maptools::spRbind(sp, sp_2)
    
  }
  
  # Return
  sp_combine
  
}


#' @rdname sp_bind
#' 
#' @export
sp_bind_many <- function (sp_list, progress = TRUE) {
  
  # Class check
  if (class(sp_list) != "list") {
    stop("The input must be list of spatial objects")
  }
  
  # Points can be easily bound with do.call
  if (grepl("point", class(sp_list[[1]]), ignore.case = TRUE)) {
    # rbind
    sp_bind <- do.call("rbind", sp_list)
    
  } else {
    # Set-up progress bar
    if (progress) {
      pb <- txtProgressBar(min = 0, max = length(sp_list), style = 3)
    }
    
    for (i in seq_along(sp_list)) {
      
      if (i == 1) {
        # The first loop, just bind the first two objects
        sp_bind <- sp_bind(sp_list[[1]], sp_list[[2]])
        
      } else {
        # Need to jump over the second element in sp_list for i = 2
        k <- i + 1
        
        # k is i + 1 so need to pass the final k as it will not exist
        if (k <= length(sp_list)) {
          # Accumulate sp_bind and add the extra objects
          sp_bind <- sp_bind(sp_bind, sp_list[[k]])
          
        }
        
      }
      # Update progress bar
      if (progress) {
        setTxtProgressBar(pb, i)
      }

    }
    
  }
  
  # Return
  sp_bind
  
}


# Function to randomly sample n features in a spatial object. 
#' @export
sp_sample_n <- function (sp, n) {
  sp <- sp[sample(nrow(sp), n), ]
  sp
}


# Function to get ids from spatial objects
#' @export
sp_feature_ids <- function (sp) {
  
  # Polygons
  if (grepl("polygon", class(sp), ignore.case = TRUE)) {
    ids <- sapply(slot(sp, "polygons"), slot, "ID")
    
  }
  
  # Lines
  if (grepl("line", class(sp), ignore.case = TRUE)) {
    ids <- sapply(slot(sp, "lines"), slot, "ID")
    
  }
  
  # Return
  ids
  
}

