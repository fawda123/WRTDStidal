######
#' Get WRTDS predictions from interpolation grids
#' 
#' Get model predictions from WRTDS using linear interpolation of values in grids
#' 
#' @param tidal_in input tidal object
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to estimate predicted values of chlorophyll from the interpolation grid.  The estimated values are based on a linear interpolation of the two predicted chlorophyll values in the interpolation grid that are bounded by the upper and lower bounds of salinity for the observed salinity at each observation.  
#' 
#' @return Appends columns to the data.frame of predicted chlorophyll value for each quantile.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' res <- chlpred(tidobj)
chlpred <- function(tidal_in, ...) UseMethod('chlpred')

#' @rdname chlpred
#'
#' @export
#'
#' @method chlpred tidal
chlpred.tidal <- function(tidal_in, ...){
  
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
    to_pred <- tidal_in$salff
    fit_grd <- cbind(to_pred, fit_grd)
    
    preds <- apply(fit_grd, 1, 
      
      function(x){
      
        # find bounding salinity value
        min_sal <- rev(which(x['to_pred'] >= sal_grd))[1]
        max_sal <- which(x['to_pred'] <= sal_grd)[1]
        
        # get bounding salinity values
        bnd_sal <- c(sal_grd[min_sal], x['to_pred'], sal_grd[max_sal])
        
        # get bounding chl values
        bnd_chl <- c(x[min_sal + 1], NA, x[max_sal + 1])
        
        chk <- unique(na.omit(bnd_chl))
        
        # if sal value is min or max in chl grid...
        if(length(chk) == 1){ 
          
          est <- chk
        
        # otherwise interpolate
        } else {
  
          est <- try(approx(bnd_sal, bnd_chl, bnd_sal)$y[2])
          
        }
   
        return(est)
      
    })        

    # append to tidal_in object
    tidal_in$fits <- preds
    names(tidal_in)[grepl('^fits$', names(tidal_in))] <- tau[i]
    
  }
  
  # exit function
  return(tidal_in)
    
}



