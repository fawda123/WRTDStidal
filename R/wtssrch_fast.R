#' Quickly find the optimal half-window width combination
#'
#' Quickly find the optimal half-window width combination to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param wins_in starting list of window weights for initializing the search algorithm
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details This is very similar to \code{\link{wtssrch}} but this function provides a quicker alternative by using the \code{\link[stats]{optim}} function to minimize the error returned by \code{\link{wrtdscv}} for a given window combination.
#' 
#' @seealso \code{\link{wrtdscv}}, \code{\link{wtssrch}}
#' 
#' @return Some stuff
#' 
#' @examples 
#' \dontrun{
#' # setup parallel backend
#' library(doParallel)
#' ncores <- detectCores() - 1  
#' registerDoParallel(cores = ncores)
#' 
#' # run search function using default search grid - takes a while
#' res <- wtssrch_fast(tidobjmean)
#' }
wtssrch_fast <- function(dat_in, ...) UseMethod('wtssrch_fast')

#' @rdname wtssrch_fast
#'
#' @export
#' 
#' @import foreach
#' 
#' @method wtssrch_fast default
wtssrch_fast.default <- function(dat_in, wins_in = NULL, ...){
  
  strt <- Sys.time()
  
  if(is.null(wins_in)) wins_in <- c(0.5, 10, 0.5)
  
  fun_in <- function(wins_in, ...){
    wins_in <- as.list(wins_in)
    wrtdscv(dat_in, wins_in, ...)
  }
  
  optim(
    wins_in, 
    fun_in, 
    method = 'L-BFGS-B', 
    lower = c(0.2, 1, 0.2), 
    upper = c(2, 15, 2)
    )
  
  print(Sys.time() - strt)
  
}
