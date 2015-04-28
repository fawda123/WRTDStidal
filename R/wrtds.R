######
#' Get WRTDS prediction grid
#'
#' Get WRTDS prediction grid for chlorophyll observations in a tidal object
#'
#' @param tidal_in input tidal object
#' @param sal_div numeric indicating number of divisions across the range of salinity to create the interpolation grid
#' @param tau numeric vector indicating conitional quantiles to fit in the weighted regression, can be many
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
#' res <- wrtds(chldat)
#' 
#' ## as tidal object
#' tidal_in <- tidal(chldat)
#' res <- wrtds(tidal_in)
#' 
#' ## multiple quantiles
#' res <- wrtds(chldat, tau = c(0.1, 0.5, 0.9))
#' }
wrtds <- function(tidal_in, sal_div = 10, tau = 0.5, trace = TRUE, ...) UseMethod('wrtds')

#' @rdname wrtds
#'
#' @export
#'
#' @method wrtds data.frame
wrtds.data.frame <- function(tidal_in, sal_div = 10, tau = 0.5, trace = TRUE, ...){
  
  dat <- tidal(tidal_in)
  wrtds(dat, sal_div, tau, trace, ...)
  
}

#' @rdname wrtds
#' 
#' @import quantreg
#' 
#' @export
#'
#' @method wrtds tidal
wrtds.tidal <- function(tidal_in, sal_div = 10, tau = 0.5, trace = TRUE, ...){
  
  #salinity values to estimate
  sal_grd <- seq(
    min(tidal_in$sal, na.rm = TRUE), 
    max(tidal_in$sal, na.rm = TRUE), 
    length = sal_div
    )

  # sort
  tau <- sort(tau)
  
  # output for predictions
  fit_grds <- matrix(nrow = nrow(tidal_in), ncol = sal_div)
  fit_grds <- replicate(length(tau), fit_grds, simplify = FALSE)
  names(fit_grds) <- paste0('fit', tau)

  if(trace){
    txt <- paste(tau, collapse = ', ')
    txt <- paste0('\nEstimating interpolation grids for tau = ', txt, ', % complete...\n\n')
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
      
      # crq model, estimates all quants
      mod <- quantreg::crq(
        Surv(chla, not_cens, type = "left") ~ 
          dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
        weights = ref_wts,
        data = tidal_in, 
        method = "Portnoy"
        )
      
      # crq doesn't always work
      test <- try({coef(mod)})
      if('try-error' %in% class(test)) next
        
      # fitted coefficients for quantile
      parms <- data.frame(coef(mod, tau))
      
      # predicted values by quantile model coefficients
      fits <- sapply(seq_along(tau), function(x){
        with(pred_dat, 
          parms[1, x] + parms[2, x] * dec_time + parms[3, x] * sal + parms[4, x] * sin(2*pi*dec_time) + parms[5, x] * cos(2*pi*dec_time)
        )
      })
      names(fits) <- names(fit_grds)
      
      # save output to grids
      for(val in tau){
        fit_ind <- paste0('fit', val)
        fit_grds[[fit_ind]][row, i] <- fits[fit_ind]
      }
    
    }
    
  }
  
  # half-window widths for attributes
  ref_wts <- getwts(tidal_in, ref_in, wins_only = TRUE, ...)
  
  # add grids to tidal object, return
  attr(tidal_in, 'half_wins') <- ref_wts
  attr(tidal_in, 'fits') <- fit_grds
  attr(tidal_in, 'sal_grd') <- sal_grd
  
  if(trace) cat('\n')
  
  return(tidal_in)
  
}
