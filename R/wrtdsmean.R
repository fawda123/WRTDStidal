######
#' Get WRTDS prediction grid for the mean response
#'
#' Get WRTDS prediction grid for the mean response of chlorophyll observations in a tidal object. The approach is similar to the \code{\link{wrtds}} function except that the mean response is modelled rather than specified quantiles.
#'
#' @param tidal_in input tidal object
#' @param sal_div numeric indicating number of divisions across the range of salinity to create the interpolation grid
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#'
#' @return Appends a regression grid as an attribute to a tidal object
#' 
#' @examples
#' \dontrun{
#' ## load data
#' data(chldat)
#' 
#' ## get wrtds grid, as data frame input
#' res <- wrtdsmean(chldat)
#' 
#' ## as tidal object
#' tidal_in <- tidal(chldat)
#' res <- wrtdsmean(tidal_in)
#' 
#' }
wrtdsmean <- function(tidal_in, sal_div = 10, trace = TRUE, ...) UseMethod('wrtdsmean')

#' @rdname wrtdsmean
#'
#' @export
#'
#' @method wrtdsmean data.frame
wrtdsmean.data.frame <- function(tidal_in, sal_div = 10, trace = TRUE, ...){
  
  dat <- tidal(tidal_in)
  wrtdsmean(dat, sal_div, trace, ...)
  
}

#' @rdname wrtdsmean
#' 
#' @export
#'
#' @method wrtdsmean tidal
wrtdsmean.tidal <- function(tidal_in, sal_div = 10, trace = TRUE, ...){
  
  #salinity values to estimate
  sal_grd <- seq(
    min(tidal_in$sal, na.rm = TRUE), 
    max(tidal_in$sal, na.rm = TRUE), 
    length = sal_div
    )
  
  # output for predictions, log-space
  fit_grds <- matrix(nrow = nrow(tidal_in), ncol = sal_div)
  fit_grds <- list(fit_grds)
  names(fit_grds) <- 'fitmean'
  
  # output for predictions, obs-space
  bt_grds <- fit_grds
  names(bt_grds) <- 'btmean'
  
  if(trace){
    txt <- '\nEstimating interpolation grid, % complete...\n\n'
    cat(txt)
    counts <- round(seq(1, nrow(tidal_in), length = 20))
  }
  
  # iterate through rows of tidal_in
  for(row in 1:nrow(tidal_in)){
    
    ref_in <- tidal_in[row, ]
    
    # progress
    if(trace){
      perc <- 5 * which(row == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    # then iterate through values in sal_grd
    for(i in seq_along(sal_grd)){
      
      ref_in$sal <- sal_grd[i]
      ref_wts <- getwts(tidal_in, ref_in, ...)
      
      # data to predict
      pred_dat <- data.frame(sal = sal_grd[i], dec_time = ref_in$dec_time)
      
      # data to model, only those w/ weights > 0
      to_mod <- tidal_in[ref_wts > 0, ]
      ref_wts <- ref_wts[ref_wts > 0]
      
      # parametric survival mod
      mod <- survival::survreg(
        survival::Surv(chla, not_cens, type = "left")
          ~ dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time),
        weights = ref_wts,
        data = to_mod, 
        dist = 'gaussian'
        )
      
      # test if model worked
      test <- try({coef(mod)})
      if('try-error' %in% class(test)) next
      
      # predicted values, log-space
      fits <-predict(
        mod,
        newdata = pred_dat
        )
      names(fits) <- names(fit_grds)
      
      # predicted values, observed
      btfits <- exp((mod$scale^2)/2)
      btfits <- btfits * exp(fits)
      names(btfits) <- names(bt_grds)
      
      # save output to grid
      fit_ind <- names(fit_grds)
      fit_grds[[fit_ind]][row, i] <- fits[fit_ind]
      bt_ind <- names(bt_grds)
      bt_grds[[bt_ind]][row, i] <- btfits[bt_ind]
    
    }
    
  }
  
  # half-window widths for attributes
  ref_wts <- getwts(tidal_in, ref_in, wins_only = TRUE, ...)
  
  # add grids to tidal object, return
  attr(tidal_in, 'half_wins') <- ref_wts
  attr(tidal_in, 'fits') <- fit_grds
  attr(tidal_in, 'bt_fits') <- bt_grds
  attr(tidal_in, 'sal_grd') <- sal_grd
  
  if(trace) cat('\n')
  
  return(tidal_in)
  
}
