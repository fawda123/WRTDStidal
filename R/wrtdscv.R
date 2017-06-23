#' Use k-fold cross-validation to evaluate WRTDS model fit
#'
#' Use k-fold cross-validation to evaluate WRTDS model fit based on supplied half-window widths.
#'
#' @param dat_in input tidal or tidalmean object
#' @param wins list of input half-window widths of the order months, years, and salinity/flow, passed to \code{\link{getwts}}
#' @param k number of folds to evaluate
#' @param seed_val seed to keep the same dataset divisions between window width comparisons
#' @param trace logical indicating if progress is printed in the console
#' @param ... arguments passed to \code{\link{wrtds}} or \code{\link{getwts}}, e.g., \code{tau = 0.2} if a \code{tidal} object is used for \code{dat_in}
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @details Default number of folds is ten.  Each fold can be evaluated with multiple cores if a parallel back end is created prior to running the function (see the examples).  This will greatly increase processing speed unless k is set to a small number. 
#' 
#' @return Overall error is the average of all errors for each fold.     
#' 
#' @seealso \code{\link{getwts}}, \code{\link{wtsplot}}, \code{\link{winsrch_grid}}, \code{\link{winsrch_optim}}
#' 
#' @examples
#' \dontrun{
#' 
#' library(doParallel)
#' ncores <- detectCores() - 1  
#' registerDoParallel(cores = ncores)
#' 
#' # half-window widths to evaluate
#' # months, years, and salinity/flow
#' wins <- list(0.5, 10, 0.5) 
#' 
#' # get ocv score for k = 10
#' wrtdscv(tidobjmean, wins = wins)
#' 
#' # get ocv score k = 2, tau = 0.2 
#' wrtdscv(tidobj, wins = wins, tau = 0.2)
#'}
wrtdscv <- function(dat_in, ...) UseMethod('wrtdscv')


#' @rdname wrtdscv
#' 
#' @import caret
#' 
#' @export
#'
#' @method wrtdscv default
wrtdscv.default <- function(dat_in, wins, k = 10, seed_val = 123, trace = TRUE, ...){
  
  # check if only one tau, if provided
  if('tau' %in% names(list(...)))
    if(length(list(...)$tau) != 1)
      stop('Only one quantile can be evaluated')
  
  set.seed(seed_val)
  
  strt <- Sys.time()
  
  if(trace) cat('Trying', unlist(wins), '\n')
  
  # create row indices for folds
  folds <- createFolds(1:nrow(dat_in), k = k)
  
  # vector to fill with cvs for each fold
  cvs <- numeric(k)

  # model eval with each fold
  errs <- foreach(i = 1:k, .export = c('wrtds', 'respred'), .packages = 'WRTDStidal') %dopar% {

    # training and test datasets 
    dat_trn <- sort(unlist(folds[-i]))
    dat_trn <- dat_in[dat_trn, ]
    dat_tst <- dat_in[folds[[i]], ]
    
    # args to pass to wrtds
    args <- c(list(dat_in = dat_trn, wins = wins, trace = trace), list(...))

    # model on training
    mod <- do.call(wrtds, args)
   
    # predictions on test
    prd_tst <- respred(mod, dat_tst[, c('date', 'flo')], trace = FALSE, omit = FALSE)

    # residual, cv score for the sample
    rsd <- na.omit(dat_tst[, 'res'] - prd_tst[, grep('^fit', names(prd_tst))])
    err <- sum(rsd^2)/length(rsd)
    
    return(err)
    
  }

  # average of all
  errs <- unlist(errs)
  out <- mean(errs)
  
  if(trace){
    tocat <- paste('Fold ', 1:k, format(errs, digits = 3), '\n')
    cat('\n', tocat)
    cat('\nOverall error', out, '\n')
    print(Sys.time() - strt)
    cat('\n')
  }
  
  return(out)
  
}