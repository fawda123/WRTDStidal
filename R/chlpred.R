######
#' Get WRTDS predictions from interpolation grids
#' 
#' Get model predictions from WRTDS using linear interpolation of values in grids
#' 
#' @param dat_in input tidal or tidalmean object
#' @param dat_pred optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to estimate predicted values of chlorophyll from the interpolation grids.  The estimated values are based on a linear interpolation of the two predicted chlorophyll values in the interpolation grid that are bounded by the upper and lower bounds of salinity for the observed salinity at each observation.  
#' 
#' @return Appends columns to the data.frame for the predicted chlorophyll values.  For tidal objects, Columns are named starting with the prefix `fit', e.g., `fit0.5' are the predicted values for the fit through the median.  For tidalmean objects, predicted values are appended for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `fits_mean' and `btfits_mean'.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get fitted values for each quantile
#' res <- chlpred(tidobj)
#' 
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get predicted values
#' res <- chlpred(tidobjmean)
#' 
chlpred <- function(dat_in, ...) UseMethod('chlpred')

#' @rdname chlpred
#'
#' @export
#'
#' @method chlpred tidal
chlpred.tidal <- function(dat_in, dat_pred = NULL, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  sal_grd <- attr(dat_in, 'sal_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace) cat('\nEstimating chlorophyll predictions\n')
  
  # quantiles to predict
  tau <- names(fits)
  
  # data to predict, uses dat_in if dat_pred is NULL
  # also creates empty list for output if not NULL
  if(is.null(dat_pred)){ to_pred <- dat_in
  } else{
    out_pred <- vector('list', length(tau))
    names(out_pred) <- tau
    to_pred <- dat_pred
  }
  to_pred <- to_pred[, c('sal', 'month', 'year')]
  
  # get predictions for each quantile
  for(i in seq_along(tau)){
    
    # interp grids
    fit_grd <- fits[[i]]
  
    preds <- apply(to_pred, 1, 
      
      function(x){
        
        # id the month, year column in the fit_grd that 
        # corresponds to the month, year of the observation
        sel <- fit_grd$year == x['year'] & fit_grd$month == x['month']
        row_in <- fit_grd[sel, -c(1, 2)]
        sal_pred <- x['sal']
        
        # interp the chlororophyll value in row_in form the corresponding sal value
        chlinterp(row_in, sal_pred, sal_grd)
    
    })          

    if(is.null(dat_pred)){
      # append to dat_in object
      dat_in$fits <- preds
      names(dat_in)[grepl('^fits$', names(dat_in))] <- tau[i]
    } else {
      out_pred[[i]] <- preds
    }
    
  }
  
  # exit function, return original object with fits, otherwise data frame of predicted values from input
  if(is.null(dat_pred)){ 
    return(dat_in)
  } else {
    out_pred <- do.call('cbind', out_pred)
    out_pred <- data.frame(dat_pred, out_pred)
    return(out_pred)
  }
    
}

#' @rdname chlpred
#'
#' @export
#'
#' @method chlpred tidalmean
chlpred.tidalmean <- function(dat_in, dat_pred = NULL, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  sal_grd <- attr(dat_in, 'sal_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace) cat('\nEstimating chlorophyll predictions\n')
  
  # interp grids
  fit_grd <- fits[[1]]
  btfit_grd <- bt_fits[[1]]
  
  # data to predict, uses dat_in if dat_pred is NULL
  if(is.null(dat_pred)) to_pred <- dat_in
  else to_pred <- dat_pred
  to_pred <- to_pred[, c('sal', 'month', 'year')]
  
  preds <- apply(to_pred, 1, 
    
    function(x){

      # id the month, year column in the fit_grd, bt_fit_grd that 
      # corresponds to the month, year of the observation
      sel <- fit_grd$year == x['year'] & fit_grd$month == x['month']
      row_in <- fit_grd[sel, -c(1, 2)]
      bt_row_in <- btfit_grd[sel, -c(1, 2)]
      sal_pred <- x['sal']
      
      # interp the chlororophyll value in row_in form the corresponding sal value
      out <- chlinterp(row_in, sal_pred, sal_grd)
      bt_out <- chlinterp(bt_row_in, sal_pred, sal_grd)
      
      c(out, bt_out)
    
  })      

  # return results for optional supplied data 
  if(!is.null(dat_pred)){
    out <- as.data.frame(t(preds))
    names(out) <- c('fits', 'bt_fits')
    out <- data.frame(dat_pred, out)
    return(out)
  }
  
  # append to dat_in object
  dat_in$fits <- preds[1, ]
  dat_in$bt_fits <- preds[2, ]

  # exit function
  return(dat_in)
    
}





