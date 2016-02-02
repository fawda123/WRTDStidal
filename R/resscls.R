######
#' Get the scale parameters for predicted values
#' 
#' Get the scale parameters for predicted values of the response variable, only applies to \code{\link{tidalmean}} objects.
#' 
#' @param dat_in input tidalmean object
#' @param dat_pred optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to get scale parameters for predicted values of the response variable from the interpolation grids.  The values are based on a bilinear interpolation of the four predicted response values at two salinity and two date values nearest to the observed salinity and date values to predict.  
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
  sal_grd <- attr(dat_in, 'sal_grd')
  
  # sanity checks
  if(is.null(scls)) stop('No scls attribute, run wrtds function')
  
  # interp grids
  scl_grd <- scls[[1]]
  
  # data to predict, uses dat_in if dat_pred is NULL
  if(is.null(dat_pred)) to_pred <- dat_in
  else to_pred <- dat_pred
  to_pred <- to_pred[, c('sal', 'date')]
  
  preds <- apply(to_pred, 1, 
    
    function(x){

      # interp the scl paramter for given date, sal in to_pred
      out <- resinterp(x['date'], x['sal'], scl_grd, sal_grd)
      out
    
  })      

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





