######
#' Get salinity normalized WRTDS predictions from interpolation grids
#' 
#' Get normalized model predictions from WRTDS to remove the effect of salinity on chlorophyll.  Predicted values in the interpolation grids are averaged across dates.
#' 
#' @param tidal_in input tidal object
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
chlnorm.tidal <- function(tidal_in, ...){
  
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
    
    # find all salff values across years here
    to_pred <- tidal_in$salff
    fit_grd <- cbind(to_pred, fit_grd)

    preds <- apply(fit_grd, 1, 
      
      function(x){
        
        ##>>>>>>>>.................. fix this
      
    })        

    # append to tidal_in object
    tidal_in$norm <- preds
    names(tidal_in)[grepl('^norm$', names(tidal_in))] <- tau[i]
    
  }
  
  # exit function
  return(tidal_in)
    
}