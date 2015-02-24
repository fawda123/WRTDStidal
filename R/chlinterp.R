######
#' Interpolate from a fit grid
#' 
#' Interpolate from a vector of predicted chlorophyll values for a given salinity value (as fraction of freshwater).  The chlorophyll values are a single observation (row) from a fitted interpolation grid that represents the predicted chlorophyll values across a set range of salinity values.
#'
#' @param row_in vector of observations from a row of an interpolation grid
#' @param sal_pred value of salff for interpolating chlorophyll
#' @param sal_grd original vector of salff values used to create interpolation grid
#' @param ... arguments passed to additional methods
#' 
#' @return Predicted chlorophyll values from the interpolation
#' 
#' @details This function is primarily used within \code{\link{chlpred}} and \code{\link{chlnorm}}.
#' 
#' @export
#' 
#' @examples
#' ##
#' 
#' # load data
#' data(tidobj)
#' 
#' # get a row from the interpolation grid to predict
#' fit_grd <- attr(tidobj, 'fits')
#' fit_grd <- fit_grd[['fit0.5']]
#' row_in <- fit_grd[1, ]
#' 
#' # get salinity values used to create interpolation grid
#' sal_grd <- attr(tidobj, 'sal_grd')
#' 
#' # get observed salinity value to inteprolate chlorophyll
#' sal_pred <- tidobj[1, 'salff']
#' 
#' # interpolate chlorophyll from an observed salinity value
#' chlinterp(row_in, sal_pred, sal_grd)
#' 
#' # error if salinity value out of range
#' \dontrun{
#' chlinterp(row_in, 0.7, sal_grd)
#' }
chlinterp <- function(row_in, ...) UseMethod('chlinterp')

#' @rdname chlinterp
#'
#' @export
#'
#' @method chlinterp numeric
chlinterp.numeric <- function(row_in, sal_pred, sal_grd, ...){
  
  # find bounding salinity value
  min_sal <- rev(which(sal_pred >= sal_grd))[1]
  max_sal <- which(sal_pred <= sal_grd)[1]
  
  # stop if salinity values out of range
  if(any(is.na(c(min_sal, max_sal))))
    stop('Value of salinity out of range for interpolation grid')
  
  # get bounding salinity values
  bnd_sal <- c(sal_grd[min_sal], sal_pred, sal_grd[max_sal])
  
  # get bounding chl values 
  bnd_chl <- c(row_in[min_sal], NA, row_in[max_sal])
  
  chk <- unique(na.omit(bnd_chl))
  
  # if sal value is min or max in chl grid...
  if(length(chk) == 1){ 
    
    est <- chk
  
  # otherwise interpolate
  } else {

    # try to interpolate if enough values
    est <- try({
      approx(bnd_sal, bnd_chl, bnd_sal)$y[2]
      }, silent = T)
    
  }

  # est is NA if approx failed
  if(!'numeric' %in% class(est)) est <- NA
  
  return(est)

}