#' Evaluate half-window width combinations 
#'
#' Evaluate a grid of half-window width combinations to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param grid_in optional input matrix of half-window widths created with \code{\link{createsrch}}, a default search grid is used if no input
#' @param parallel logical indicating if function is executed with multiple cores, the parallel backend must be setup prior to running the function (see the examples).  
#' @param trace logical indicating if progress is saved to a text file in the working directory
#' @param seed_val seed to keep the same dataset divisions between window width comparisons
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @seealso \code{\link{createsrch}}, \code{\link{wrtdscv}}
#' 
#' @return A data frame of the search grid with associated errors for each cross-validation result.  Errors for each grid row are averages of all errors for each fold used in cross-validation. 
#' 
#' The process can be very time consuming.  A forty year dataset of monthly observations will take a few hours to evaluate using the default settings.   
#' 
#' @examples 
#' \dontrun{
#' # setup parallel backend
#' library(doParallel)
#' ncores <- detectCores() - 1  
#' registerDoParallel(cores = ncores)
#' 
#' # run search function using default search grid - takes a while
#' res <- wtssrch(tidobjmean)
#' 
#' # optimal combo
#' res[which.min(res$err), ]
#' 
#' # create a custom search grid, e.g. years only
#' grid_in <- createsrch(mos = 1, yrs = seq(1, 10), sal = 1)
#' 
#' res <- wtssrch(tidobjmean, grid_in)
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
    set.seed(seed_val)
    cvslamb <- wrtdscv(dat_in, wins = grid_in[i, ], trace = FALSE, ...)
    
    return(cvslamb)
    
  }
  
  out <- data.frame(grid_in, err = unlist(res))
  
  return(out)
  
}
