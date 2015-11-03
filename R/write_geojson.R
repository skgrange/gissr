#' Function to write GeoJSON files with usage analogous to \code{write.table}.
#' 
#' @param sp Spatial object to be written as GeoJSON file. 
#' @param file File name of GeoJSON file. 
#' @param pretty Format the JSON for readability.  
#' 
#' @author Stuart K. Grange
#' 
#' @examples
#' \dontrun{
#' # Export spatial object as GeoJSON file
#' write_geojson(sp_thames_locks, "thames_locks.json")
#'
#' }
#' 
#' @export 
write_geojson <- function (sp, file, pretty = TRUE) {
  
  # Make json string
  json <- geojsonio::geojson_json(sp, pretty = pretty)
  
  # Write string to disk
  write(json, file)
  
}
