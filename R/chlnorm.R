######
#' Get salinity normalized WRTDS predictions from interpolation grids
#' 
#' Get normalized model predictions from WRTDS to remove the effect of salinity on chlorophyll.  Predicted values in the interpolation grids are averaged across dates.
#' 
#' @param tidal_in input tidal object
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to normalize predicted values of chlorophyll from the interpolation grid of each estimated quantile.  The normalized values are based on the average of all predicted chlorophyll estimates across the range of salinity values that have occurred on the same date throughout each year.  For example, normalized values for July 2000 are the mean predicted chlorophyll at that date using the observed salinity values that occur in July of all years.  The normalized values allow an interpretation of chlorophyll trends that are independent of changes in salinity or freshwater inputs.  
#' 
#' @return Appends columns to the data.frame of normalized chlorophyll values for each quantile.  Columns are named starting with the prefix `norm', e.g., `norm0.5' are the normalized values for the fit through the median.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get fitted values for each quantile
#' res <- chlnorm(tidobj)
#' 
#' ##
#' \dontrun{
#' # fails if fits attribute not available
#' data(chldat)
#' chldat <- tidal(chldat)
#' chlnorm(chldat)
#' }
chlnorm <- function(tidal_in, ...) UseMethod('chlnorm')

#' @rdname chlnorm
#'
#' @export
#'
#' @method chlnorm tidal
chlnorm.tidal <- function(tidal_in, trace = TRUE, ...){
  
  fits <- attr(tidal_in, 'fits')
  sal_grd <- attr(tidal_in, 'sal_grd')
  num_obs <- nrow(tidal_in)
  
  # stop if no fits attribute
  if(is.null(fits)) stop('No fits attribute in the tidal object, run wrtds function')
  
  if(trace) cat('\nNormalizing chlorophyll predictions\n\n')
  
  # quantiles to predict
  tau <- names(fits)
  
  # normalize predictions for each quantile
  for(i in seq_along(tau)){
  
    # interp grid and salff values to interp
    fit_grd <- fits[[i]]

    norms <- rep(NA_real_, num_obs)
    for(row in 1:num_obs){

      # get salff values across all dates for the row
      sal_vals <- salfind(tidal_in, row)
      
      # get interpolated values for each salff value
      row_in <- fit_grd[row, ]
      
      # use chlinterp for all sal_vals 
      chlpreds <- sapply(sal_vals, 
        function(x) chlinterp(row_in, x, sal_grd)
        ) 
      
      # average for normalization and append to norms
      norms[row] <- mean(chlpreds)
      
    }    

    # append to tidal_in object for the grid
    tidal_in$norm <- norms
    colnm <- gsub('^fit', 'norm', tau[i])
    names(tidal_in)[grepl('^norm$', names(tidal_in))] <- colnm
    
  }
  
  # exit function
  return(tidal_in)
    
}