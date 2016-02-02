######
#' Interpolate from a fit grid
#' 
#' Interpolate the response variable in two dimensions from the fit grid, where the first dimension is salinity and the second is date.
#'
#' @param date_in obseved data to interpolate for the response variable
#' @param sal_in observed salinity value to interpolate for the response variable
#' @param fit_grd interpolation grid created from \code{\link{wrtds}}
#' @param sal_grd original vector of salinity values used to create interpolation grid
#' @param ... arguments passed to additional methods
#' 
#' @return Predicted values of the response variable from the interpolation
#' 
#' @details This function is used in \code{\link{respred}} and \code{\link{resnorm}}.
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
#' # get observed salinity value to interpolate the response
#' date_in <- tidobj$date[1]
#' sal_in <- tidobj$sal[1]
#' 
#' # interpolate the response from an observed date and salinity value
#' resinterp(date_in, sal_in, fit_grd, sal_grd)
resinterp <- function(date_in, sal_in, ...) UseMethod('resinterp')

#' @rdname resinterp
#'
#' @export
#'
#' @method resinterp default
resinterp.default <- function(date_in, sal_in, fit_grd, sal_grd, ...){

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
  
  # get bounding salinity, date values, corresponding response values
  bnd_dts <- with(fit_grd, c(date[min_dts], date[max_dts]))
  bnd_sal <- c(sal_grd[min_sal], sal_grd[max_sal])
  not_sal <- grep('^X', names(fit_grd), invert = T)
  bnd_res <- try({fit_grd[c(min_dts, max_dts), length(not_sal) + c(min_sal, max_sal)]})
  
  # fails if sal_in outside of range
  if(class(bnd_res) %in% 'try-error') return(NA) 
  # fails if date_in outside of range
  # fails if missing values in interp grids, these will only occur on the edges
  if(any(is.na(c(bnd_dts, bnd_res)))) return(NA) 
 
  # bilinear interpolation, across salinity then across dates    
  sal1 <- approx_uni(bnd_sal, bnd_res[1, ], sal_in)
  sal2 <- approx_uni(bnd_sal, bnd_res[2, ], sal_in)
  ests <- approx_uni(bnd_dts, c(sal1, sal2), date_in)

  return(ests)

}
