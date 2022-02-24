#' Function to split spatial lines into segments with fixed lengths. 
#' 
#' \code{sp_segment_lines} has been pulled from another package which can be
#' found \href{https://github.com/jmt2080ad/polylineSplitter}{here}, 
#' 
#' @param sp Spatial line object. 
#' 
#' @param length Length for the split segments. This is in the units of 
#' \code{sp}'s projection system. 
#' 
#' @param features Should the function be applied to all features in \code{sp}
#' separately?  
#' 
#' @author Jason Taylor, David Blodgett, and Stuart K. Grange
#' 
#' @seealso \url{https://github.com/jmt2080ad/polylineSplitter}
#' 
#' @return SpatialLinesDataFrame with one \code{id} variable. 
#' 
#' @examples 
#' 
#' # Make a line, this is in Switzerland so am using the local, metric projection 
#' # system
#' sp_lines <- dplyr::tribble(
#'   ~latitude,  ~longitude,
#'   47.381114,  8.5769906, 
#'   47.3839327, 8.5831275, 
#'   47.3884799, 8.5849729, 
#'   47.3902377, 8.5856166, 
#'   47.3910512, 8.5865822, 
#'   47.3930994, 8.5912171, 
#'   47.3938402, 8.5924187, 
#'   47.3951621, 8.5935345, 
#'   47.3975152, 8.5952726, 
#'   47.3993162, 8.5954228, 
#'   47.4057356, 8.598856
#' ) %>% 
#'   sp_from_data_frame(type = "lines") %>% 
#'   sp_transform(projection_swiss())
#' 
#' # Check length
#' sp_length(sp_lines)
#' 
#' # Into 100 metre sections
#' sp_lines_split <- sp_segment_lines(sp_lines, length = 100)
#' 
#' # Check lengths
#' sp_length(sp_lines_split, features = TRUE)
#' sp_length(sp_lines_split, features = FALSE)
#' 
#' @export
sp_segment_lines <- function(sp, length, features = FALSE) {
  
  # Check input
  stopifnot(stringr::str_detect(sp_class(sp), "Lines"))
  
  # Save projection string, it is stripped in this function
  projection <- sp_projection(sp)
  
  # Add data slot if needed
  if (!stringr::str_detect(sp_class(sp), "Data")) {
    sp <- sp_promote(sp)
    names(sp)[1] <- "id_for_splitting"
  }
  
  # Vectorise function if needed
  if (features) {
    
    # Add splitting variable
    sp$id_for_splitting <- seq_len(length(sp))
    
    # Do for every feature
    sp <- sp %>% 
      split(.$id_for_splitting) %>% 
      purrr::map(sp_segment_lines_worker, length = length) %>% 
      sp_bind()
    
  } else {
    # A single call
    sp <- sp_segment_lines_worker(sp, length = length)
  }
  
  # Force projection
  sp <- sp_transform(sp, to = projection, warn = FALSE)
  
  return(sp)
  
}


sp_segment_lines_worker <- function(sp, length) {
  
  # To a tibble
  df_sp <- sp_fortify(sp) %>% 
    select(y = latitude,
           x = longitude)
  
  # Split into a list of tibbles with lengths calculated
  spoints <- split_df(df_sp, length)
  
  linelist <- list()
  lineslist <- list()
  id <- 1
  j <- 1
  
  for (i in 1:(nrow(spoints) - 1)) {
    
    suppressWarnings(
      linelist[j] <- Line(spoints[c(i, i + 1), c(1:2)])
    )
    
    j = j + 1
    
    if (spoints[i + 1, 3] == 1) {
      
      suppressWarnings(
        lineslist[id] <- Lines(linelist, ID = id)
      )
      
      id = id + 1
      linelist <- list()
      j = 1
      
    }
    
  }
  
  # Back to spatial object
  sp <- SpatialLinesDataFrame(
    SpatialLines(lineslist), 
    data = data.frame(id = 0:(length(lineslist) - 1))
  )
  
  return(sp)
  
}


split_df <- function(xydf, dist) {

  x <- c()
  y <- c()
  end <- c()
  rem <- 0
  for(i in 1:nrow(xydf)){
    if(i == 1){
      x <- c(x, xydf$x[i])
      y <- c(y, xydf$y[i])
      end <- c(end,1)
    }
    if(i != 1){
      cx <- xydf$x[i] - xydf$x[i-1]
      cy <- xydf$y[i] - xydf$y[i-1]
      if(cx & cy != 0){
        len <- sqrt((cx^2) + (cy^2)) + rem
        segs <- len %/% dist
        if(segs == 0){
          rem <- len
        }
        else{
          m <- cx/cy
          ymod <- dist / (sqrt((m^2)+1))
          xmod <- ymod * abs(m)
          yremsub <- rem / (sqrt((m^2)+1))
          xremsub <- yremsub * abs(m)
          xmod <- modck(cx, xmod)
          ymod <- modck(cy, ymod)
          xremsub <- modck(cx, xremsub)
          yremsub <- modck(cy, yremsub)
          xnew <- seq(xydf$x[i-1] - xremsub, xydf$x[i-1] + (xmod * segs), by = xmod)[-1]
          ynew <- seq(xydf$y[i-1] - yremsub, xydf$y[i-1] + (ymod * segs), by = ymod)[-1]
          if(length(xnew) != length(ynew)){
            if(abs(length(xnew) - length(ynew)) > 1) stop("Error found in new sequence. Code needs to be reviewed.")
            if(length(xnew) < length(ynew)){
              xnew <- c(xnew, xydf$x[i-1] + (xmod * segs))
            } else {
              ynew <- c(ynew, xydf$y[i-1] + (ymod * segs))
            }
          }
          rem<-sqrt((xydf$x[i] - tail(xnew,1))^2 + (xydf$y[i] - tail(ynew,1))^2)
          x <- c(x, xnew)
          y <- c(y, ynew)
          end <- c(end, rep(1, length(xnew)))
        }
      }
      if(cx != 0 & cy == 0){
        len <- cx + rem
        segs <- len %/% dist
        if(segs == 0){
          rem <- len
        }
        else{
          xmod <- dist
          ymod <- 0
          xmod <- modck(cx, xmod)
          xremsub <- modck(cx, xremsub)
          yremsub <- 0     
          xnew <- seq(xydf$x[i-1] - rem, xydf$x[i-1] + (xmod * segs), by = xmod)[-1]
          ynew <- rep(xydf$y[i-1], segs)
          rem <- xydf$x[i] - tail(xnew,1)
          x <- c(x, xnew)
          y <- c(y, ynew)
          end <- c(end, rep(1, length(xnew)))
        }
      }
      if(cx == 0 & cy != 0){
        len <- cy + rem
        segs <- len %/% dist
        if(segs == 0){
          rem <- len
        }
        else{
          xmod <- 0
          ymod <- dist
          xmod <- modck(cx, xmod)
          xremsub <- modck(cx, xremsub)
          yremsub <- 0    
          xnew <- rep(xydf$x[i-1], segs) 
          ynew <- seq(xydf$y[i-1] - rem, xydf$y[i-1] + (ymod * segs), by = ymod)[-1]
          rem <- xydf$y[i] - tail(ynew,1)
          x <- c(x, xnew)
          y <- c(y, ynew)
          end <- c(end, rep(1, length(ynew)))
        }
      }
      x <- c(x, xydf$x[i])
      y <- c(y, xydf$y[i])
      if(i != nrow(xydf)){
        end <- c(end, 0)    
      }
      else{
        end <- c(end, 1) 
      }
    }
  }
  return(data.frame(x = x, y = y, end = end))
}


modck <- function(change, mod) {
  if(change<0){
    return(mod*-1)
  }
  else{
    return(mod)
  }
}
