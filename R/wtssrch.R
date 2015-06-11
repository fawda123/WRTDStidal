#' Evaluate half-window width combinations 
#'
#' Evaluate a grid of half-window width combinations to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param grid_in optional input matrix of half-window widths created with \code{\link{createsrch}}, a default search grid is used if no input
#' @param parallel logical indicating if function is executed with multiple cores
#' @param min_obs logical passed to \code{\link{getwts}} indicating if a minimum number of observations with non-zero weights should be used in the evaluations, default being not to impose this restriction for a more valid comparison of window-widths
#' @param trace logical indicating if progress is saved to a text file in the working directory
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @seealso \code{\link{createsrch}}, \code{\link{wrtdscv}}
#' 
#' @examples 
#' \dontrun{
#' library(doParallel)
#' ncores <- detectCores() - 1  
#' registerDoParallel(cores = ncores)
#' 
#' res <- wtssrch(tidobjmean)
#' }
wtssrch <- function(dat_in, ...) UseMethod('wtssrch')

#' @rdname wtssrch
#'
#' @export
#' 
#' @import foreach
#' 
#' @method wtssrch default
wtssrch.default <- function(dat_in, grid_in = NULL, parallel = TRUE, trace = TRUE, min_obs = FALSE, ...){
  
  if(is.null(grid_in)) grid_in <- createsrch()
  
  strt <- Sys.time()
  
  res <- foreach(i = 1:nrow(grid_in), .export = 'wtsevalplot', .packages = 'WRTDStidal') %dopar% {
    
    # progress
    if(trace){
      if(!parallel){
        cat(i, ' of ', nrow(grid_in), '\n')
      } else {
        sink('log.txt')
        cat('Log entry time', as.character(Sys.time()), '\n')
        cat(i, ' of ', nrow(grid_in), '\n')
        print(Sys.time() - strt)
        sink()
      }
    }
    
    # get ocv score from model
    ocvout <- wtsevalplot(dat_in, grid_in[i, ], ocv = TRUE, plot_out = FALSE, 
      trace = FALSE, min_obs = min_obs)
    
    return(ocvout)
    
  }
  
  res <- do.call('rbind', res)$value
  out <- data.frame(grid_in, ocvsc = res)
  
  return(out)
  
}
