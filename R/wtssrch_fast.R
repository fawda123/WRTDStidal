#' Quickly find the optimal half-window width combination
#'
#' Quickly find the optimal half-window width combination to use for weighted regression
#' 
#' @param dat_in input data object to use with weighted regression
#' @param lower vector of minimum half-window widths to evaluate
#' @param upper vector of maximum half-window widths to evaluate
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details This is very similar to \code{\link{wtssrch}} but this function uses \code{\link[stats]{optim}} to minimize the error returned by \code{\link{wrtdscv}} for a given window combination.  The search algorithm uses the limited-memory modification of the BFGS quasi-Newton method to impose upper and lower limits on the optimization search. These limits can be changed using the \code{lower} and \code{upper} arguments. 
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
#' @import foreach rgenoud
#' 
#' @method wtssrch_fast default
wtssrch_fast.default <- function(dat_in, lower = c(0.2, 2, 0.2), upper = c(2, 15, 2), ...){
  
  strt <- Sys.time()
  
  # starting 
  wins_in <- c(5, 10, 5)
  lower[c(1, 3)] <- lower[c(1, 3)] * 10
  upper[c(1, 3)] <- upper[c(1, 3)] * 10
  
  fun_in <- function(wins_in, ...){
    wins_in[c(1, 3)] <- wins_in[c(1, 3)]/10
    wins_in <- as.list(wins_in)
    wrtdscv(dat_in, wins_in, ...)
  }
  
  out <- genoud(
    fun_in,
    nvars = 3, 
    starting.values = wins_in,
    data.type.int = TRUE,
    Domains = cbind(lower, upper)
    )
  
  print(Sys.time() - strt)
  
  return(out)
  
}
