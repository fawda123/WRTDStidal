#' Get trend for a single time period
#' 
#' Get trend for a single time period
#'
#' @param x numeric of year values
#' @param y norm numeric of annually averaged response variable
#' @param single logical if trends are based on only first and last year of aggregated, i.e., no averaging of first/last three
#' 
#' @details Estimates trends using methods described in \code{\link{wrtdstrnd}}.  Used internally and is not to be called by the user. Function runs on individual time period groups defined by arguments in \code{\link{wrtdstrnd}}.
#' 
chngest <- function(x, y, single = F){

  tomod <- data.frame(x, y)
  tomod <- tomod[order(tomod$x), ]
  
  # get only first and last year's data
  if(single){
  
    srt <- tomod[which.min(tomod$x), 'y']
    stp <- tomod[which.max(tomod$x), 'y']
  
  # otherwise get average of first and last three years of data
  } else {
    
    srt <- which.min(tomod$x)
    srt <- seq(srt, srt + 2)
    srt <- mean(tomod[srt, 'y'], na.rm = TRUE)
    stp <- which.max(tomod$x)
    stp <- seq(stp - 2, stp)
    stp <- mean(tomod[stp, 'y'], na.rm = TRUE)
      
  }
  
  # get percent change from beginning to end
  out <- 100 * (stp - srt)/srt
  return(out)
  
}

