#' Functions to write GeoJSON files with usage analogous to \code{write.table}.
#' 
#' \code{write_geojson_js} is a simple extension which adds a \code{var} to the 
#' GeoJSON string so leaflet and other JavaScript libraries can assign the 
#' objects easily. 
#' 
#' @param sp Spatial object to be written as GeoJSON file. 
#' 
#' @param file File name of GeoJSON file. 
#' 
#' @param name Name of JavaScript object (only used in \code{write_geojson_js}). 
#' 
#' @param pretty Format the JSON for readability. 
#' 
#' @param round How many decimal points should the coordinate pairs be exported
#' with? Default is maximum but \code{6} is common for spatial data. This argument
#' is currently ignored due to an error with \code{geojsonio::geojson_json}, 
#' to-do: fix when I can. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' 
#' # Export spatial object as GeoJSON file
#' write_geojson(sp_thames_locks, "thames_locks.json", pretty = TRUE)
#' 
#' # Export spatial object as GeoJSON file ready for leaflet
#' write_geojson(sp_thames_locks, "thames_locks.js", name = "data_locks")
#'
#' }
#' 
#' @export 
write_geojson <- function(sp, file, pretty = TRUE, round = NA) {
  
  # Create
  json <- create_geojson(sp, pretty, round)
  
  # Write string to disk
  write(json, file)
  
}


#' @rdname write_geojson
#' 
#' @export 
write_geojson_js <- function(sp, file, name = NA, pretty = TRUE, round = NA) {
  
  # A name
  if (is.na(name[1])) name <- "spatial_object"
  
  # Create
  json <- create_geojson(sp, pretty, round)
  
  # Add the js formatting for an object
  json_js <- stringr::str_c("var ", name, " = [", json, "];")
  
  # Write string to disk
  write(json_js, file)
  
}


# No export
create_geojson <- function(sp, pretty = TRUE, round = NA) {
  
  # Make json string, will also work for data frames sometimes but will give
  # message
  # , digits = round, removed, bug in geojson_json parsing the argument
  json <- suppressMessages(
    geojsonio::geojson_json(sp)
  )
  
  # Use jsonlite to do a better job of pretty printing, expensive though
  if (pretty) {
    
    # Max precision is needed here
    json <- jsonlite::fromJSON(json)
    json <- jsonlite::toJSON(json, pretty = TRUE, auto_unbox = TRUE, 
                             digits = NA)
    
  }
  
  # Return
  json
  
}
