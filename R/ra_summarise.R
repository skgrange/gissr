#' Function to summarise each raster layer and return results in a tibble. 
#' 
#' @author Stuart K. Grange
#' 
#' @param ra Raster object.
#' 
#' @param f Summary function. \code{mean} is the default. 
#' 
#' @param na.rm Should \code{NA}s be omitted? 
#' 
#' @param by_layer Should each layer be summarised? 
#' 
#' @param variable_as_date Are the variables/names of the \code{\link{ra_drill}}
#' output dates? If so, the dates will be parsed and the variable renamed. 
#' 
#' @param tz If \code{variable_as_date} is \code{TRUE}, what time zone are the 
#' dates stored in? This will almost certainly be \code{UTC}.
#' 
#' @return Tibble. 
#' 
#' @export
ra_summarise <- function(ra, f = mean, na.rm = TRUE, by_layer = TRUE,
                         variable_as_date = FALSE, tz = "UTC") {
  
  # Calculate the summary for each layer
  if (by_layer) {
    
    # Calculate the summaries, this is a named vector
    x <- raster::cellStats(ra, stat = f, na.rm = na.rm)
    
    # Make tibble
    if (inherits(x, "matrix")) {
      
      # Get names of layers
      variable <- colnames(x)
      
      # Format matrix as a long tibble
      df <- x %>% 
        t() %>% 
        as_tibble(.name_repair = "minimal") %>% 
        purrr::set_names(1:ncol(.)) %>% 
        mutate(variable = !!variable) %>% 
        tidyr::pivot_longer(-variable, names_to = "value_index") %>% 
        mutate(value_index = as.integer(value_index))
      
    } else {
      # When just a named numeric vector
      df <- x %>% 
        tibble::enframe() %>% 
        purrr::set_names(c("variable", "value"))
    }
    
    # Switch variable into a date
    if (variable_as_date && stringr::str_detect(df$variable[1], "^X")) {
      df <- df %>% 
        rename(date = variable) %>% 
        mutate(date = stringr::str_remove(date, "^X"), 
               date = lubridate::ymd_hms(date, tz = tz, truncated = 3))
    }
    
  } else {
    stop("Not implemented.", call. = FALSE)
    # raster::calc
  }
  
  return(df)
  
}
