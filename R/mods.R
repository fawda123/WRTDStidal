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
#' @examples
#' ##
#' 
chlpred <- function(tidal_in, ...) UseMethod('chlpred')

#' @rdname chlpred
#'
#' @export
#'
#' @method chlpred tidal
chlpred.tidal <- function(tidal_in, ...){
  
  fits <- attr(tidal_in, 'fits')
  
  if(is.null(fits)) stop('No fits attribute in the tidal object, run wrtds function')
  
  
    
    #observation to normalize
    ref.in<-dat.in[row,]
    
    #get inteprolation grid data
    grd.in<-sal.grd[sal.grd$seg == seg & sal.grd$dec.time==ref.in$dec.time,]
    
    #matching closest values in grid to actual input
    grd.mtch<-grd.in[which.min(abs(grd.in$sal.grid-ref.in$sal.ref)),c('fit.md','fit.hi','fit.lo','b.md','b.hi','b.lo','var.md','var.hi','var.lo','bt.md','bt.hi','bt.lo')]
    
    obs[as.numeric(row.names(grd.mtch))]<-1
    
    #output for segment
    seg.fit<-rbind(seg.fit,cbind(ref.in,grd.mtch))
    
    }
  
  #output for all
  fit<-rbind(fit,seg.fit)
  
  }



