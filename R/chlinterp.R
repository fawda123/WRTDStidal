######
#' Interpolate from a fit grid
#' 
#' Interpolate chlorophyll in two dimensions from the fit grid, where the first dimension is salinity and the second is date.
#'
#' @param date_in obseved data to interpolate for chlorophyll
#' @param sal_in observed salinity value to interpolate for chlorophyll
#' @param fit_grd interpolation grid created from \code{\link{wrtds}}
#' @param sal_grd original vector of salinity values used to create interpolation grid
#' @param ... arguments passed to additional methods
#' 
#' @return Predicted chlorophyll values from the interpolation
#' 
#' @details This function is used in \code{\link{chlpred}} and \code{\link{chlnorm}}.
#' 
#' @export
#' 
#' @examples
#' ##
#' 
#' # load data
#' data(tidobj)
#' 
#' # interpolation grid from the model
#' fit_grd <- attr(tidobj, 'fits')
#' fit_grd <- fit_grd[['fit0.5']]
#' 
#' # salinity values used for interpolation grid
#' sal_grd <- attr(tidobj, 'sal_grd')
#' 
#' # get observed salinity value to inteprolate chlorophyll
#' date_in <- tidobj$date[1]
#' sal_in <- tidobj$sal[1]
#' 
#' # interpolate chlorophyll from an observed date and salinity value
#' chlinterp(date_in, sal_in, fit_grd, sal_grd)
chlinterp <- function(date_in, sal_in, ...) UseMethod('chlinterp')

#' @rdname chlinterp
#'
#' @export
#'
#' @method chlinterp default
chlinterp.default <- function(date_in, sal_in, fit_grd, sal_grd, ...){

  # date as date class if not
  if(!inherits(date_in, 'date'))
    date_in <- as.Date(date_in, format = '%Y-%m-%d')
  if(!inherits(date_in, 'numeric'))
    sal_in <- as.numeric(sal_in)
  
  # find bounding salinity value
  min_sal <- rev(which(sal_in >= sal_grd))[1]
  max_sal <- which(sal_in <= sal_grd)[1]
  
  # find bounding date value
  min_dts <- rev(which(date_in >= fit_grd$date))[1]
  max_dts <- which(date_in <= fit_grd$date)[1]
  
  # get bounding salinity, date values, corresponding chl values
  bnd_dts <- with(fit_grd, c(date[min_dts], date[max_dts]))
  bnd_sal <- c(sal_grd[min_sal], sal_grd[max_sal])
  not_sal <- grep('^X', names(fit_grd), invert = T)
  bnd_chl <- fit_grd[c(min_dts, max_dts), length(not_sal) + c(min_sal, max_sal)]
  
  # bilinear interpolation, across salinity then across dates    
  sal1 <- approx_uni(bnd_sal, bnd_chl[1, ], sal_in)
  sal2 <- approx_uni(bnd_sal, bnd_chl[2, ], sal_in)
  ests <- approx_uni(bnd_dts, c(sal1, sal2), date_in)

  return(ests)

}
