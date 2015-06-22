######
#' Get salinity normalized WRTDS predictions from interpolation grids
#' 
#' Get normalized model predictions from WRTDS to remove the effect of salinity on chlorophyll.  Predicted values in the interpolation grids are averaged across dates.
#' 
#' @param dat_in input tidal or tidalmean object
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to normalize predicted values of chlorophyll from the interpolation grid for each model.  The normalized values are based on the average of all predicted chlorophyll estimates across the range of salinity values that have occurred on the same date throughout each year.  For example, normalized values for July 2000 are the mean predicted chlorophyll at that date using the observed salinity values that occur in July of all years.  The normalized values allow an interpretation of chlorophyll trends that are independent of changes in salinity or freshwater inputs.  
#' 
#' @return Appends columns to the data.frame for normalized chlorophyll values.  For, tidal objects, columns are named starting with the prefix `norm', e.g., `norm0.5' are the normalized values for the fit through the median.  For tidalmean objects, columns are appended for the log-transformed and back-transformed normalized values, named `norm' and `bt_norm'.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get flow-normalized values for each quantile
#' res <- chlnorm(tidobj)
#'
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get flow-normalized values
#' res <- chlnorm(tidobjmean)
#' 
chlnorm <- function(dat_in, ...) UseMethod('chlnorm')

#' @rdname chlnorm
#'
#' @export
#'
#' @method chlnorm tidal
chlnorm.tidal <- function(dat_in, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  sal_grd <- attr(dat_in, 'sal_grd')
  num_obs <- nrow(dat_in)
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')

  if(trace) cat('\nNormalizing chlorophyll predictions\n\n')
  
  # quantiles to predict
  tau <- names(fits)
  
  # normalize predictions for each quantile
  for(i in seq_along(tau)){
  
    # interp grid and sal values to interp
    fit_grd <- fits[[i]]

    norms <- rep(NA_real_, num_obs)
    for(row in 1:num_obs){

      # get sal values across all dates for the row
      sal_vals <- salfind(dat_in, row)
      
      # use chlinterp for all sal_vals 
      chlpreds <- sapply(sal_vals, 
        function(x) chlinterp(dat_in[row, 'date'], x, fit_grd, sal_grd)
        ) 
      
      # average for normalization and append to norms
      norms[row] <- mean(chlpreds)
      
    }    

    # append to dat_in object for the grid
    dat_in$norm <- norms
    colnm <- gsub('^fit', 'norm', tau[i])
    names(dat_in)[grepl('^norm$', names(dat_in))] <- colnm
    
  }
  
  # exit function
  return(dat_in)
    
}

#' @rdname chlnorm
#'
#' @export
#'
#' @method chlnorm tidalmean
chlnorm.tidalmean <- function(dat_in, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  sal_grd <- attr(dat_in, 'sal_grd')
  num_obs <- nrow(dat_in)

  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')

  if(trace) cat('\nNormalizing chlorophyll predictions\n\n')

  # prep interp grids by adding month, year columns
  fit_grd <- fits[[1]]
  btfit_grd <- bt_fits[[1]]

  norms <- rep(NA_real_, num_obs)
  btnorms <- norms
  for(row in 1:num_obs){

    # get sal values across all dates for the row
    sal_vals <- salfind(dat_in, row)
    
    # use chlinterp for all sal_vals 
    chlpreds <- sapply(sal_vals, 
      function(x){
      
        nrms <- chlinterp(dat_in[row, 'date'], x, fit_grd, sal_grd)
        btnrms <- chlinterp(dat_in[row, 'date'], x, btfit_grd, sal_grd)
        c(nrms, btnrms)
        
      }
    ) 
    
    # average for normalization and append to norms, btnorms
    norms[row] <- mean(chlpreds[1, ])
    btnorms[row] <- mean(chlpreds[2, ])
      
    # append to dat_in object for the grid
    dat_in$norm <- norms
    dat_in$bt_norm <- btnorms
    
  }
  
  # exit function
  return(dat_in)
    
}