#' Find the optimal half-window width combination
#'
#' Find the optimal half-window width combination to use for weighted regression.
#' 
#' @param dat_in input data object to use with weighted regression
#' @param wins_in starting list of window weights for initializing the search algorithm
#' @param control A list of control parameters passed to \code{\link[stats]{optim}} (see details in \code{\link[stats]{optim}} help file).  The value passed to \code{factr} controls the convergence behavior of the \code{"L-BFGS-B"} method.  Values larger than the default will generally speed up the optimization with a potential loss of precision. \code{parscale} describes the scaling values of the parameters.
#' @param lower vector of minimum half-window widths to evaluate
#' @param upper vector of maximum half-window widths to evaluate
#' @param ... arguments passed to \code{\link{wrtdscv}}, \code{\link{wrtds}}, or \code{\link{getwts}}
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
winsrch_optim.default <- function(dat_in, wins_in = NULL, control = list(factr = 1e7, parscale = c(1, 10, 1)), lower = c(0.1, 1, 0.1), upper = c(2, 15, 2), ...){
  
  strt <- Sys.time()

  # creates args to pass down the function chain
  if(is.null(wins_in)) wins_in <- c(0.5, 10, 0.5)
    
  fun_in <- function(wins_in, min_obs = min_obs){
    wins_in <- as.list(wins_in)
    args <- c(list(dat_in = dat_in, wins = wins_in), list(...))
    do.call(wrtdscv, args)
  }
  
  out <- optim(
    wins_in, 
    fun_in, 
    method = 'L-BFGS-B', 
    lower = lower, 
    upper = upper, 
    control = control
  )
  
  out$elapsed <- Sys.time() - strt
  
  return(out)
  
}
