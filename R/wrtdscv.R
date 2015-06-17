#' Use k-fold cross-validation to evaluate WRTDS model fit
#'
#' Use k-fold cross-validation to evaluate WRTDS model fit based on supplied half-window widths.
#'
#' @param dat_in input model object
#' @param wins_in list of input half-window widths of the order months, years, and salinity, passed to \code{\link{getwts}}
#' @param k number of folds to evaluate
#' @param seed_val seed to keep the same dataset divisions between window width comparisons
#' @param trace logical indicating if progress is printed in the console
#' @param ... arguments passed to or from other methods (e.g., \code{\link{getwts}})
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
#' # months, years, and salinity
#' wins <- list(0.5, 10, 0.5) 
#' 
#' #get ocv score for k = 10
#' wrtdscv(tidobjmean, wins_in = wins)
#' 
#'}
wrtdscv <- function(dat_in, ...) UseMethod('wrtdscv')

#' @rdname wrtdscv
#' 
#' @import caret
#' 
#' @export
#'
#' @method wrtdscv tidalmean
wrtdscv.tidalmean <- function(dat_in, wins_in, k = 10, seed_val = 123, trace = TRUE, ...){
  
  set.seed(seed_val)
  
  strt <- Sys.time()
  
  if(trace) cat('Trying', unlist(wins_in), '\n')
  
  # create row indices for folds
  folds <- createFolds(1:nrow(dat_in), k = k)
  
  # vector to fill with cvs for each fold
  cvs <- numeric(k)
  
  # model eval with each fold
  errs <- foreach(i = 1:k, .export = c('wrtds', 'chlpred'), .packages = 'WRTDStidal') %dopar% {
    
    # training and test datasets 
    dat_trn <- sort(unlist(folds[-i]))
    dat_trn <- dat_in[dat_trn, ]
    dat_tst <- dat_in[folds[[i]], ]
    
    # model on training
    mod <- wrtds(dat_trn, wins = wins_in, trace = FALSE, ...)
    
    # predictions on test
    prd_tst <- chlpred(mod, dat_tst, trace = FALSE)

    # residual, cv score for the sample
    res <- na.omit(with(prd_tst, chla - fits))
    err <- sum(res^2)/length(res)
    
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