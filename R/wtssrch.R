#' Evaluate half-window width combinations 
#'
#' Evaluate a grid of half-window width combinations to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param grid_in optional input matrix of half-window widths created with \code{\link{createsrch}}, a default search grid is used if no input
#' @param parallel logical indicating if function is executed with multiple cores
#' @param trace logical indicating if progress is saved to a text file in the working directory
#' @param seed_val seed passed to \code{\link{wrtdscv}} to keep the same dataset divisions between window width comparisons
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
wtssrch.default <- function(dat_in, grid_in = NULL, parallel = TRUE, trace = TRUE, seed_val = 123, ...){
  
  if(is.null(grid_in)) grid_in <- createsrch()
  
  strt <- Sys.time()
  
  res <- foreach(i = 1:nrow(grid_in), .export = 'wrtdscv', .packages = 'WRTDStidal') %dopar% {
    
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
    
    # get cv score for lambda (window comb)
    cvslamb <- wrtdscv(dat_in, wins = grid_in[i, ], trace = FALSE, seed_val = seed_val, ...)
    
    return(cvslamb)
    
  }
  
  browser()
  res <- do.call('rbind', res)$value
  out <- data.frame(grid_in, ocvsc = res)
  
  return(out)
  
}
