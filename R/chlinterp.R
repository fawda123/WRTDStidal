######
#' Interpolate from a fit grid
#' 
#' Interpolate from a fit grid for a given salinity value (as fraction of freshwater) and observation (row)
#'
#' @param fit_in matrix of fitted values for a given quantile, added to \code{\link{tidal}} object from \code{\link{wrtds}} function
#' @param salval vector of values of salff to predict
#' @param row_in row number of \code{fit_in} to use
#'
#' @return Predicted chlorophyll values from the interpolation
#' 
#' @details This function is primarily used within \code{\link{chlprd}} and \code{\link{chlnorm}}.
chlinterp <- function(fit_in, salval, row_in){
  
  # row to use
  x <- fit_in[row_in, ]
  
  ##....................... fix this
  
  # find bounding salinity value
  min_sal <- rev(which(sal_val >= sal_grd))[1]
  max_sal <- which(sal_val <= sal_grd)[1]
  
  # get bounding salinity values
  bnd_sal <- c(sal_grd[min_sal], sal_val, sal_grd[max_sal])
  
  # get bounding chl values
  bnd_chl <- c(x[min_sal + 1], NA, x[max_sal + 1])
  
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