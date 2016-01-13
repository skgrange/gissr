sp_buffer <- function (sp, features, width) {
  
  sp <- rgeos::gBuffer(sp, byid = features, width = width)
  
  # Return
  sp
  
}