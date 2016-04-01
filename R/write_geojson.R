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
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Export spatial object as GeoJSON file
#' write_geojson(sp_thames_locks, "thames_locks.json", pretty = TRUE)
#' 
#' # Export spatial object as GeoJSON file ready for leaflet
#' write_geojson(sp_thames_locks, "thames_locks.js", name = "data_locks")
#'
#' }
#' 
#' @export 
write_geojson <- function(sp, file, pretty = TRUE) {
  
  # Make json string
  json <- geojsonio::geojson_json(sp, pretty = pretty)
  
  # Write string to disk
  write(json, file)
  
}


#' @rdname write_geojson
#' 
#' @export 
write_geojson_js <- function (sp, file, name, pretty = TRUE) {
  
  # Make json string
  json <- geojsonio::geojson_json(sp, pretty = pretty)
  
  # Add the js formatting for an object
  js <- stringr::str_c("var ", name, " = [", json, "];")
  
  # Write string to disk
  write(js, file)
  
}
