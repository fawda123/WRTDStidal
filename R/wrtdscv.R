#' Use k-fold cross-validation to evaluate WRTDS model fit
#'
#' Use k-fold cross-validation to evaluate WRTDS model fit based on supplied half-window widths.
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
#' @details Default number of folds is ten.
#' 
#' @return Overall error is the average of all errors for each fold.     
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
#' @method wrtdscv tidalmean
wrtdscv.tidalmean <- function(dat_in, wins_in, k = 10, trace = TRUE, ...){
  
  # create row indices for folds
  folds <- createFolds(1:nrow(dat_in), k = k)
  
  # vector to fill with cvs for each fold
  cvs <- numeric(k)
  
  # model eval with each fold
  for(i in 1:k){
    
    if(trace) cat(paste0('Fold ', i, ' of ', k, '...'))
    
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
    cvs[i] <- err
    
    if(trace) cat(' error', err, '\n')
    
  }
    
  # average all cv scores 
  out <- mean(cvs)
  if(trace) cat('Overall error', out, '\n')
  
  return(out)
  
}