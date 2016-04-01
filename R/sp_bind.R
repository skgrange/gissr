#' Functions to bind spatial objects together. 
#' 
#' \code{sp_bind} combines two similar spatial objects together with the same 
#' syntax of \code{rbind}. \code{sp_bind_many} combines multiple spatial objects 
#' together but the input must be a list. 
#' 
#' These function are useful for combining shapefiles or tables of WKT strings
#' together. The feature IDs within line- and polygon-spatial objects must be 
#' manipulated for the binding to occur. The original, non-manipulated IDs are 
#' not preserved. 
#' 
#' @param sp Spatial object one. 
#' 
#' @param sp_2 Spatial object two.
#' 
#' @param sp_list A list containing two or more spatial objects. 
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
sp_bind <- function(sp, sp_2) {
  
  # Class check
  if (!class(sp) == class(sp_2)) {
    stop("Geometries must be of the same type to be bound.", .call = FALSE)
  }
  
  # Create a list
  sp_list <- list(sp, sp_2)
  
  # Bind
  sp_bind <- sp_bind_many(sp_list)

  # Return
  sp_bind
  
}


#' @rdname sp_bind
#' 
#' @export
sp_bind_many <- function(sp_list) {
  
  # Class check
  if (!is.list(sp_list)) stop("Input must be list.", .call = FALSE)
  
  # Points can be easily bound with do.call
  if (grepl("point", class(sp_list[[1]]), ignore.case = TRUE)) {
    
    sp_bind <- sp_list_bind(sp_list)
    
  } else {
    
    # Reset ids, this will use uuids for uniqueness
    sp_list <- sp_reset_feature_ids(sp_list)
    
    # Bind
    sp_bind <- sp_list_bind(sp_list)
    
    # Reset ids again to something suitable, a character vector
    sp_bind <- sp_reset_feature_ids(sp_bind)
  
  }
  
  # Return
  sp_bind
  
}


# Bind objects using do.call
sp_list_bind <- function(sp_list) do.call("rbind", sp_list)


# Function to randomly sample n features in a spatial object. 
#' @export
sp_sample_n <- function(sp, n) sp[sample(nrow(sp), n), ]

