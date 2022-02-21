#' Function to pad a data frame/tibble's coordinates. 
#' 
#' @param df Data frame/tibble with \code{latitude} and \code{longitude} 
#' variables. 
#' 
#' @param resolution Resolution to pad coordinates to in metres. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble.
#' 
#' @export
pad_coordinates <- function(df, resolution = 1) {
  
  # Check inputs
  stopifnot(c("latitude", "longitude") %in% names(df))
  stopifnot(resolution >= 1)
  
  # Calculate the distances
  df <- df %>% 
    mutate(
      distance = gissr::distance_by_haversine(latitude, longitude, unit = "metres"),
      distance = if_else(is.na(distance), 0, distance),
      distance_sum = cumsum(distance),
      distance_sum = round(distance_sum)
    )
  
  # Create sequence
  distance_sequence <- seq(from = 0, to = max(df$distance_sum), by = 1)
  
  # Pad and interpolate
  df_pad <- tibble(distance_sum = !!distance_sequence) %>% 
    left_join(df, by = "distance_sum") %>% 
    mutate(across(c("latitude", "longitude"), threadr::na_interpolate, na.rm = TRUE))
  
  # Aggregate if desired
  if (resolution != 1) {
    df_pad <- df_pad %>%
      mutate(
        distance_sum = threadr::round_any(
          distance_sum, accuracy = resolution, f = floor
        )
      ) %>% 
      group_by(distance_sum) %>% 
      summarise(across(c("latitude", "longitude"), mean, na.rm = TRUE))
  }
  
  # Drop distances
  df_pad <- df_pad %>% 
    select(latitude,
           longitude)
  
  return(df_pad)
  
}
