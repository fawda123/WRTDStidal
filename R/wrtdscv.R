#' Use k-fold cross-validation to evaluate WRTDS model fit
#'
#' Use k-fold cross-validation to evaluate WRTDS model fit
#'
#' @param dat_in input model object
#' @param wins_in list of input half-window widths of the order months, years, and salinity, passed to \code{\link{getwts}}
#' @param k number of folds to evaluate
#' @param trace logical indicating if progress is printed in the console
#' @param ... arguments passed to or from other methods (e.g., \code{\link{getwts}})
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @details Some details
#' 
#' @return some more crap
#' 
#' @seealso \code{\link{getwts}}, \code{\link{wtsplot}}
#' 
#' @examples
#' \dontrun{
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
#' @method wrtdscv default
wrtdscv.default <- function(dat_in, wins_in, k = 10, trace = TRUE, ...){

  folds <- createFolds(1:nrow(dat_in), k = k)
  
  for(i in 1:k){
    
    if(trace) cat('Fold', k, '\n')
    
    # training and test datasets 
    dat_trn <- sort(unlist(folds[-i]))
    dat_trn <- dat_in[dat_trn, ]
    dat_tst <- dat_in[folds[[i]], ]
    
    # model on training
    mod <- wrtds(dat_trn, wins = wins_in)
    
    # predictions on test
    prd_tst <- chlpred(mod, dat_tst)
    
    # residual
    res <- with(dat_tst, chla - prd_tst$chla)
    
    
    
  }
    
    
}