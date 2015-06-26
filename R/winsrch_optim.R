#' Find the optimal half-window width combination
#'
#' Find the optimal half-window width combination to use for weighted regression.
#' 
#' @param dat_in input data object to use with weighted regression
#' @param wins_in starting list of window weights for initializing the search algorithm
#' @param factr numeric value controlling the convergence behavior of the \code{"L-BFGS-B"} method of \code{\link[stats]{optim}}.  Values larger than the default will generally speed up the optimization with a potential loss of precision.
#' @param lower vector of minimum half-window widths to evaluate
#' @param upper vector of maximum half-window widths to evaluate
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details This function uses \code{\link[stats]{optim}} to minimize the error returned by \code{\link{wrtdscv}} for a given window combination.  The search algorithm uses the limited-memory modification of the BFGS quasi-Newton method to impose upper and lower limits on the optimization search. These limits can be changed using the \code{lower} and \code{upper} arguments. 
#' 
#' @seealso \code{\link{wrtdscv}}, \code{\link{winsrch_grid}}
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
#' # run search function - takes a while
#' res <- winsrch_optim(tidobjmean)
#' }
winsrch_optim <- function(dat_in, ...) UseMethod('winsrch_optim')

#' @rdname winsrch_optim
#'
#' @export
#' 
#' @method winsrch_optim default
winsrch_optim.default <- function(dat_in, wins_in = NULL, factr = 1e7, lower = c(0.1, 1, 0.1), upper = c(2, 15, 2), ...){
  
  strt <- Sys.time()
  
  if(is.null(wins_in)) wins_in <- c(0.5, 10, 0.5)
  
  fun_in <- function(wins_in, ...){
    wins_in <- as.list(wins_in)
    wrtdscv(dat_in, wins_in, ...)
  }
  
  out <- optim(
    wins_in, 
    fun_in, 
    method = 'L-BFGS-B', 
    lower = lower, 
    upper = upper, 
    control = list(
      factr = factr,
      parscale = c(1, 10, 1))
    )
  
  out$elapsed <- Sys.time() - strt
  
  return(out)
  
}
