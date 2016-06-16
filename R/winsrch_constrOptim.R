#' Find the optimal half-window width combination
#'
#' Find the optimal half-window width combination to use for weighted regression.  This differs from \code{\link{winsrch_optim}} by using \code{\link[stats]{constrOptim}}
#' 
#' @param dat_in input data object to use with weighted regression
#' @param wins_in starting list of window weights for initializing the search algorithm
#' @param control A list of control parameters passed to \code{\link[stats]{optim}} (see details in \code{\link[stats]{optim}} help file).
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
winsrch_constrOptim <- function(dat_in, ...) UseMethod('winsrch_constrOptim')

#' @rdname winsrch_constrOptim
#'
#' @export
#' 
#' @method winsrch_constrOptim default
winsrch_constrOptim.default <- function(dat_in, wins_in = NULL, control = list(), lower = c(0.1, 1, 0.1), upper = c(2, 15, 2), ...){
  
  strt <- Sys.time()

  # creates args to pass down the function chain
  if(is.null(wins_in)) wins_in <- c(0.5, 10, 0.5)
    
  fun_in <- function(wins_in, min_obs = min_obs){
    wins_in <- as.list(wins_in)
    args <- c(list(dat_in = dat_in, wins = wins_in), list(...))
    do.call(wrtdscv, args)
  }
  
  # setup box constraints in right format
  # Constraints
  bounds <- cbind(lower, upper)
  colnames(bounds) <- c("lower", "upper")
  
  # Convert the constraints to the ui and ci matrices
  n <- nrow(bounds)
  ui <- rbind( diag(n), -diag(n) )
  ci <- c( bounds[,1], - bounds[,2] )
  
  # Remove the infinite values
  i <- as.vector(is.finite(bounds))
  ui <- ui[i,]
  ci <- ci[i]

  # run the algorithm
  out <- constrOptim(
    wins_in, 
    fun_in, 
    grad = NULL,
    ui = ui, 
    ci = ci,
    control = control
  )
  
  out$elapsed <- Sys.time() - strt
  
  return(out)
  
}
