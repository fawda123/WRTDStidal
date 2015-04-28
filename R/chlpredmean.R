######
#' Get WRTDS predictions for the mean model from interpolation grids
#' 
#' Get model predictions from WRTDS for the mean model using linear interpolation of values in grids
#' 
#' @param tidal_in input tidal object
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtdsmean} to estimate predicted values of chlorophyll from the interpolation grid of the mean model.  The estimated values are based on a linear interpolation of the two predicted chlorophyll values in the interpolation grid that are bounded by the upper and lower bounds of salinity for the observed salinity at each observation.  
#' 
#' @return Appends columns to the data.frame of predicted chlorophyll values for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `fits_mean' and `btfits_mean'.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobjmean)
#' 
#' # get predicted values
#' res <- chlpredmean(tidobjmean)
#' 
#' ##
#' \dontrun{
#' # fails if incorrect input
#' data(chldat)
#' chldat <- tidal(chldat)
#' chlpredmean(chldat)
#' }
chlpredmean <- function(tidal_in, ...) UseMethod('chlpredmean')

#' @rdname chlpredmean
#'
#' @export
#'
#' @method chlpredmean tidal
chlpredmean.tidal <- function(tidal_in, trace = TRUE, ...){
  
  fits <- attr(tidal_in, 'fits')
  bt_fits <- attr(tidal_in, 'bt_fits')
  sal_grd <- attr(tidal_in, 'sal_grd')
  
  # stop if not the correct model
  if(is.null(bt_fits)) stop('Incorrect model, run wrtdsmean function')
  
  if(trace) cat('\nInterpolating chlorophyll predictions\n')
  
  # interp grid and sal values to interp
  fit_grd <- fits[[1]]
  to_pred <- tidal_in$sal
  fit_grd <- cbind(to_pred, fit_grd)
  
  preds <- apply(fit_grd, 1, 
    
    function(x){
    
      row_in <- x[-1]
      sal_pred <- x[1]
      
      chlinterp(row_in, sal_pred, sal_grd)
    
  })      
  
  # bt interp grid and sal values to interp
  btfit_grd <- bt_fits[[1]]
  to_pred <- tidal_in$sal
  btfit_grd <- cbind(to_pred, btfit_grd)
  
  btpreds <- apply(btfit_grd, 1, 
    
    function(x){
    
      row_in <- x[-1]
      sal_pred <- x[1]
      
      chlinterp(row_in, sal_pred, sal_grd)
    
  })    

  # append to tidal_in object
  tidal_in$fits <- preds
  tidal_in$bt_fits <- btpreds

  # exit function
  return(tidal_in)
    
}



