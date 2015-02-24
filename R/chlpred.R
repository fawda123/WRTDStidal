######
#' Get WRTDS predictions from interpolation grids
#' 
#' Get model predictions from WRTDS using linear interpolation of values in grids
#' 
#' @param tidal_in input tidal object
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to estimate predicted values of chlorophyll from the interpolation grid of each estimated quantile.  The estimated values are based on a linear interpolation of the two predicted chlorophyll values in the interpolation grid that are bounded by the upper and lower bounds of salinity for the observed salinity at each observation.  
#' 
#' @return Appends columns to the data.frame of predicted chlorophyll values for each quantile.  Columns are named starting with the prefix `fit', e.g., `fit0.5' are the predicted values for the fit through the median.
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
#' ##
#' \dontrun{
#' # fails if fits attribute not available
#' data(chldat)
#' chldat <- tidal(chldat)
#' chlpred(chldat)
#' }
chlpred <- function(tidal_in, ...) UseMethod('chlpred')

#' @rdname chlpred
#'
#' @export
#'
#' @method chlpred tidal
chlpred.tidal <- function(tidal_in, ...){
  
  fits <- attr(tidal_in, 'fits')
  sal_grd <- attr(tidal_in, 'sal_grd')
  
  # stop if no fits attribute
  if(is.null(fits)) stop('No fits attribute in the tidal object, run wrtds function')
  
  # quantiles to predict
  tau <- names(fits)
  
  # get predictions for each quantile
  for(i in seq_along(tau)){
    
    # interp grid and salff values to interp
    fit_grd <- fits[[i]]
    to_pred <- tidal_in$salff
    fit_grd <- cbind(to_pred, fit_grd)
    
    preds <- apply(fit_grd, 1, 
      
      function(x){
      
        row_in <- x[-1]
        sal_pred <- x[1]
        
        chlinterp(row_in, sal_pred, sal_grd)
      
    })        

    # append to tidal_in object
    tidal_in$fits <- preds
    names(tidal_in)[grepl('^fits$', names(tidal_in))] <- tau[i]
    
  }
  
  # exit function
  return(tidal_in)
    
}



