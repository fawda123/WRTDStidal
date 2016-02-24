######
#' Interpolate from a fit grid
#' 
#' Interpolate the response variable in two dimensions from the fit grid, where the first dimension is salinity/flow and the second is date.
#'
#' @param date_in obseved data to interpolate for the response variable
#' @param flo_in observed salinity/flow value to interpolate for the response variable
#' @param fit_grd interpolation grid created from \code{\link{wrtds}}
#' @param flo_grd original vector of salinity/flow values used to create interpolation grid
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
#' # salinity/flow values used for interpolation grid
#' flo_grd <- attr(tidobj, 'flo_grd')
#' 
#' # get observed salinity/flow value to interpolate the response
#' date_in <- tidobj$date[1]
#' flo_in <- tidobj$flo[1]
#' 
#' # interpolate the response from an observed date and salinity/flow value
#' resinterp(date_in, flo_in, fit_grd, flo_grd)
resinterp <- function(date_in, flo_in, ...) UseMethod('resinterp')

#' @rdname resinterp
#'
#' @export
#'
#' @method resinterp default
resinterp.default <- function(date_in, flo_in, fit_grd, flo_grd, ...){

  # date as date class if not
  if(!inherits(date_in, 'date'))
    date_in <- as.Date(date_in, format = '%Y-%m-%d')
  if(!inherits(flo_in, 'numeric'))
    flo_in <- as.numeric(flo_in)
  
  # find bounding salinity/flow value
  min_flo <- rev(which(flo_in >= flo_grd))[1]
  max_flo <- which(flo_in <= flo_grd)[1]
  
  # find bounding date value
  min_dts <- rev(which(date_in >= fit_grd$date))[1]
  max_dts <- which(date_in <= fit_grd$date)[1]
  
  # get bounding salinity/flow, date values, corresponding response values
  bnd_dts <- with(fit_grd, c(date[min_dts], date[max_dts]))
  bnd_flo <- c(flo_grd[min_flo], flo_grd[max_flo])
  not_flo <- grep('^X', names(fit_grd), invert = T)
  bnd_res <- try({fit_grd[c(min_dts, max_dts), length(not_flo) + c(min_flo, max_flo)]})
  
  # fails if flo_in outside of range
  if(class(bnd_res) %in% 'try-error') return(NA) 
  # fails if date_in outside of range
  # fails if missing values in interp grids, these will only occur on the edges
  if(any(is.na(c(bnd_dts, bnd_res)))) return(NA) 
 
  # bilinear interpolation, across salinity/flow then across dates    
  flo1 <- approx_uni(bnd_flo, bnd_res[1, ], flo_in)
  flo2 <- approx_uni(bnd_flo, bnd_res[2, ], flo_in)
  ests <- approx_uni(bnd_dts, c(flo1, flo2), date_in)

  return(ests)

}
