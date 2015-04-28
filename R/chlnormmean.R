######
#' Get salinity normalized WRTDS predictions from interpolation grids for the mean model
#' 
#' Get normalized model predictions from WRTDS for the mean model to remove the effect of salinity on chlorophyll.  Predicted values in the interpolation grids are averaged across dates.
#' 
#' @param tidal_in input tidal object
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtdsmean} to normalize predicted and back-transformed predicted values of chlorophyll from the interpolation grids for the mean model.  The normalized values are based on the average of all predicted chlorophyll estimates across the range of salinity values that have occurred on the same date throughout each year.  For example, normalized values for July 2000 are the mean predicted chlorophyll at that date using the observed salinity values that occur in July of all years.  The normalized values allow an interpretation of chlorophyll trends that are independent of changes in salinity or freshwater inputs.  
#' 
#' @return Appends columns to the data.frame of normalized chlorophyll values for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `nrms_mean' and `btnrms_mean'.
#'  
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobjmean)
#' 
#' # get flow-normalized values
#' res <- chlnormmean(tidobjmean)
#' 
#' ##
#' \dontrun{
#' # fails if incorrect input
#' data(chldat)
#' chldat <- tidal(chldat)
#' chlnormmean(chldat)
#' }
chlnormmean <- function(tidal_in, ...) UseMethod('chlnormmean')

#' @rdname chlnormmean
#'
#' @export
#'
#' @method chlnormmean tidal
chlnormmean.tidal <- function(tidal_in, trace = TRUE, ...){
  
  fits <- attr(tidal_in, 'fits')
  bt_fits <- attr(tidal_in, 'bt_fits')
  sal_grd <- attr(tidal_in, 'sal_grd')
  num_obs <- nrow(tidal_in)
  
  # stop if incorrect input
  if(is.null(bt_fits)) stop('Incorrect model, run wrtdsmean function')
  
  if(trace) cat('\nNormalizing chlorophyll predictions\n\n')

  # interp grid and sal values to interp
  fit_grd <- fits[[1]]
  btfit_grd <- bt_fits[[1]]

  norms <- rep(NA_real_, num_obs)
  btnorms <- norms
  for(row in 1:num_obs){

    # get sal values across all dates for the row
    sal_vals <- salfind(tidal_in, row)
  
    # get interpolated values for each sal value
    row_in <- fit_grd[row, ]
    btrow_in <- btfit_grd[row, ]
    
    # use chlinterp for all sal_vals 
    chlpreds <- sapply(sal_vals, 
      function(x){
      
        nrms <- chlinterp(row_in, x, sal_grd)
        btnrms <- chlinterp(btrow_in, x, sal_grd)
        c(nrms, btnrms)
        
      }
    ) 
    
    # average for normalization and append to norms, btnorms
    norms[row] <- mean(chlpreds[1, ])
    btnorms[row] <- mean(chlpreds[2, ])
      
    # append to tidal_in object for the grid
    tidal_in$norm <- norms
    tidal_in$bt_norm <- btnorms
    
  }
  
  # exit function
  return(tidal_in)
    
}