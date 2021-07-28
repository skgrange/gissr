#' Function to get OpenStreetMap boundaries as spatial polygons. 
#' 
#' \code{get_osm_boundary} accesses 
#' \href{http://polygons.openstreetmap.fr/index.py}{this} polygon creator tool. 
#' 
#' @param id A vector of OpenStreetMap relations. An integer key.
#' 
#' @param way Is \code{id} a way? If \code{TRUE}, a different method is needed
#' which scrapes XML directly from OpenStreetMap for nodes and is a bit slow. 
#' 
#' @param sleep Number of seconds between server queries. This is useful if 
#' many large polygons are being requested. 
#' 
#' @param verbose Should the function give messages? 
#' 
#' @return SpatialPolygonsDataFrame with WGS84 projection. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' 
#' # Get North York Moors National Park boundary
#' sp_moors <- get_osm_boundary(409150)
#' 
#' @export
get_osm_boundary <- function(id, way = FALSE, sleep = 1, verbose = FALSE) {
  
  # Parse input
  id <- stringr::str_trim(id)
  
  if (!way) {
    
    # Do
    sp_list <- purrr::map(
      id, ~osm_boundary_worker(., sleep = sleep, verbose = verbose)
    )
    
    # Check types
    class_sp <- sapply(sp_list, class)
    
    if (length(class_sp) == 1 && class_sp == "NULL") {
      sp <- NULL
    } else {
      
      # Bind list
      sp <- sp_bind(sp_list)
      
      # Drop unique observation id
      sp@data$id <- NULL
      
    }
    
  } else {
    
    # Use osm functions to directly scrape
    sp_list <- purrr::map(id, ~osm_boundary_way_worker(., verbose = verbose))
    
    # Bind list of spatial objects
    sp <- sp_bind(sp_list)
    
    sp$id <- seq(1, length(sp))
    sp$id_osm <- id
    
  }
  
  return(sp)
  
}


osm_boundary_way_worker <- function(id, verbose) {
  
  # Message for user
  if (verbose) message(threadr::date_message(), "`", id, "`...")
  
  # Get nodes
  vector_nodes <- get_osm_way_data(id)$relations
  
  # Get nodes
  df <- get_osm_node_data(vector_nodes)$attributes
  
  # Promote
  sp <- sp_from_data_frame(df, type = "polygon")
  
  return(sp)
  
}


osm_boundary_worker <- function(id, sleep, verbose) {
  
  # Message for user
  if (verbose) message(threadr::date_message(), "`", id, "`...")
  
  # Build query
  url <- stringr::str_c(
    "http://polygons.openstreetmap.fr/get_wkt.py?id=", 
    id, 
    "&params=0"
  )
  
  # Get wkt
  text <- tryCatch({
    threadr::read_lines(url)
  }, error = function(e) {
    message("'id' ", id, " not found...")
    NULL
  })
  
  # If null then return now
  if (is.null(text)) return(NULL)
  
  # Drop preamble/projection information
  text <- stringr::str_remove(text, "SRID=4326;")
  
  # Promote to spatial
  sp <- sp_from_wkt(text, verbose = FALSE)
  
  # Add projection
  sp <- sp_transform(sp, to = projection_wgs84(), warn = FALSE)
  
  # Give a data slot
  sp <- sp_promote(sp)
  
  # Add open street map id to data slot
  sp@data$id_osm <- as.integer(id)
  
  # Sleep if desired
  if (!is.na(sleep)) Sys.sleep(sleep)
  
  return(sp)
  
}
