######
#' Get the scale parameters for predicted values
#' 
#' Get the scale parameters for predicted values of the response variable, only applies to \code{\link{tidalmean}} objects.
#' 
#' @param dat_in input tidalmean object
#' @param dat_pred optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied
#' @param ... arguments passed to or from other methods
#' 
#' @import dplyr
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to get scale parameters for predicted values of the response variable from the interpolation grids.  The values are based on a bilinear interpolation of the four predicted response values at two salinity/flow and two date values nearest to the observed salinity/flow and date values to predict.  
#' 
#' @return Appends columns to the data.frame for the associated scale value for the predicted values.  A column is appended to the \code{dat_in} object, named `scls'.
#'  
#' @examples
#' ##
#'
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get predicted values
#' res <- resscls(tidobjmean)
#' 
resscls <- function(dat_in, ...) UseMethod('resscls')

#' @rdname resscls
#'
#' @export
#'
#' @method resscls tidalmean
resscls.tidalmean <- function(dat_in, dat_pred = NULL, ...){
  
  scls <- attr(dat_in, 'scls')
  
  # sanity checks
  if(is.null(scls)) stop('No scls attribute, run wrtds function')
  
  # interp grids
  scl_grd <- scls[[1]]
  dts <- scl_grd$date
  scl_grd <- select(scl_grd, -year, -month, -day, -date)
  
  # data to predict, uses dat_in if dat_pred is NULL
  if(is.null(dat_pred)) to_pred <- dat_in
  else to_pred <- dat_pred
  to_pred <- to_pred[, c('date', 'flo')]
  
  # bilinear interpolatoin of scl grid with data to predict
  preds <- interp.surface(
    obj = list(
      y = attr(dat_in, 'flo_grd'),
      x = dts,
      z = scl_grd
    ), 
    loc = to_pred
  )
  
  # return results for optional supplied data 
  if(!is.null(dat_pred)){
    out <- as.data.frame(preds)
    names(out) <- c('scls')
    out <- data.frame(dat_pred, out)
    return(out)
  }
  
  # append to dat_in object
  dat_in$scls <- preds

  # exit function
  return(dat_in)
    
}





