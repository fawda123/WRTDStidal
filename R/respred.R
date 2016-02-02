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
#' This function is used after \code{wrtds} to estimate predicted values of the response variable from the interpolation grids.  The estimated values are based on a bilinear interpolation of the four predicted response values at two salinity/flow and two date values nearest to the observed salinity/flow and date values to predict.  
#' 
#' @return Appends columns to the data.frame for the predicted values.  For tidal objects, columns are named starting with the prefix `fit', e.g., `fit0.5' are the predicted values for the fit through the median.  For tidalmean objects, predicted values are appended for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `fits' and `bt_fits'.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get fitted values for each quantile
#' res <- respred(tidobj)
#' 
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get predicted values
#' res <- respred(tidobjmean)
#' 
respred <- function(dat_in, ...) UseMethod('respred')

#' @rdname respred
#'
#' @export
#'
#' @method respred tidal
respred.tidal <- function(dat_in, dat_pred = NULL, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace) cat('\nEstimating predictions\n')
  
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
  to_pred <- to_pred[, c('flo', 'date')]
  
  # get predictions for each quantile
  for(i in seq_along(tau)){
    
    # interp grids
    fit_grd <- fits[[i]]
  
    preds <- apply(to_pred, 1, 
      
      function(x){
        
        # interp the response for given date, flo in to_pred
        resinterp(x['date'], x['flo'], fit_grd, flo_grd)
    
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

#' @rdname respred
#'
#' @export
#'
#' @method respred tidalmean
respred.tidalmean <- function(dat_in, dat_pred = NULL, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace) cat('\nEstimating predictions\n')
  
  # interp grids
  fit_grd <- fits[[1]]
  btfit_grd <- bt_fits[[1]]
  
  # data to predict, uses dat_in if dat_pred is NULL
  if(is.null(dat_pred)) to_pred <- dat_in
  else to_pred <- dat_pred
  to_pred <- to_pred[, c('flo', 'date')]
  
  preds <- apply(to_pred, 1, 
    
    function(x){

      # interp the response for given date, flo in to_pred with relevant grid
      out <- resinterp(x['date'], x['flo'], fit_grd, flo_grd)
      bt_out <- resinterp(x['date'], x['flo'], btfit_grd, flo_grd)
      
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





