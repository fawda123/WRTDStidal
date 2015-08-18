######
#' Get WRTDS prediction grid
#'
#' Get WRTDS prediction grid for chlorophyll observations in a tidal or tidalmean object
#'
#' @param dat_in input tidal or tidalmean object
#' @param sal_div numeric indicating number of divisions across the range of salinity to create the interpolation grid
#' @param tau numeric vector indicating conitional quantiles to fit in the weighted regression, can be many
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @return Appends interpolation grid attributes to the input object.  For a tidal object, this could include multiple grids for each quantile.  For tidalmean objects, only one grid is appended to the `fits' attribute, in addition to a back-transformed grid as the `bt_fits' attribute and a grid of the scale parameter of each prediction as the `scls' attribute.  Grid rows correspond to the dates in the input data.
#' 
#' @examples
#' \dontrun{
#' ## load data
#' data(chldat)
#' 
#' ## as tidal object
#' dat_in <- tidal(chldat)
#' res <- wrtds(dat_in)
#' 
#' ## as tidalmean object
#' dat_in <- tidalmean(chldat)
#' res <- wrtds(dat_in)
#' 
#' ## multiple quantiles
#' res <- wrtds(dat_in, tau = c(0.1, 0.5, 0.9))
#' }
wrtds <- function(dat_in, ...) UseMethod('wrtds')

#' @rdname wrtds
#' 
#' @import quantreg
#' 
#' @export
#'
#' @method wrtds tidal
wrtds.tidal <- function(dat_in, sal_div = 10, tau = 0.5, trace = TRUE, ...){
  
  #salinity values to estimate
  sal_grd <- seq(
    min(dat_in$sal, na.rm = TRUE), 
    max(dat_in$sal, na.rm = TRUE), 
    length = sal_div
    )

  # sort
  tau <- sort(tau)
  
  # save orig for output
  dat_out <- dat_in
  
  # output for predictions
  fit_grds <- matrix(nrow = nrow(dat_in), ncol = sal_div)
  fit_grds <- replicate(length(tau), fit_grds, simplify = FALSE)
  names(fit_grds) <- paste0('fit', tau)

  if(trace){
    txt <- paste(tau, collapse = ', ')
    txt <- paste0('\nEstimating interpolation grids for tau = ', txt, ', % complete...\n\n')
    cat(txt)
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # iterate through rows of dat_in
  for(row in 1:nrow(dat_in)){
    
    ref_in <- dat_in[row, ]
    
    # progress
    if(trace){
      perc <- 5 * which(row == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    # then iterate through values in sal_grd
    for(i in seq_along(sal_grd)){
      
      ref_in$sal <- sal_grd[i]
      ref_wts <- getwts(dat_in, ref_in, ...)
      
      # data to predict
      pred_dat <- data.frame(sal = sal_grd[i], dec_time = ref_in$dec_time)
      
      # crq model, estimates all quants
      mod <- quantreg::crq(
        survival::Surv(chla, not_cens, type = "left") ~ 
          dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
        weights = ref_wts,
        data = dat_in, 
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
  ref_wts <- getwts(dat_in, ref_in, wins_only = TRUE, ...)
  
  # add year, month, day to interp grids
  fit_grds <- lapply(fit_grds, function(x) {
    fill_grd(x, dat_in)
  })
  
  # add grids to tidal object, return
  attr(dat_out, 'half_wins') <- ref_wts
  attr(dat_out, 'fits') <- fit_grds
  attr(dat_out, 'sal_grd') <- sal_grd
  
  if(trace) cat('\n')
  
  return(dat_out)
  
}

#' @rdname wrtds
#' 
#' @export
#'
#' @method wrtds tidalmean
wrtds.tidalmean <- function(dat_in, sal_div = 10, trace = TRUE, ...){
  
  #salinity values to estimate
  sal_grd <- seq(
    min(dat_in$sal, na.rm = TRUE), 
    max(dat_in$sal, na.rm = TRUE), 
    length = sal_div
    )
  
  # save orig for output
  dat_out <- dat_in
    
  # output for predictions, log-space
  fit_grds <- matrix(nrow = nrow(dat_in), ncol = sal_div)
  fit_grds <- list(fit_grds)
  names(fit_grds) <- 'fitmean'
  
  # output for predictions, obs-space
  bt_grds <- fit_grds
  names(bt_grds) <- 'btmean'
  
  # sd of model at each point in the grid
  scl_grds <- fit_grds
  names(scl_grds) <- 'sclmean'
  
  if(trace){
    txt <- '\nEstimating interpolation grid for mean response, % complete...\n\n'
    cat(txt)
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # iterate through rows of dat_in
  for(row in 1:nrow(dat_in)){
    
    ref_in <- dat_in[row, ]
    
    # progress
    if(trace){
      perc <- 5 * which(row == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    # then iterate through values in sal_grd
    for(i in seq_along(sal_grd)){
      
      ref_in$sal <- sal_grd[i]
      ref_wts <- getwts(dat_in, ref_in, ...)
      
      # data to predict
      pred_dat <- data.frame(sal = sal_grd[i], dec_time = ref_in$dec_time)
      
      # data to model, only those w/ weights > 0
      to_mod <- dat_in[ref_wts > 0, ]
      ref_wts <- ref_wts[ref_wts > 0]
      
      # parametric survival mod
      mod <- try({survival::survreg(
        survival::Surv(chla, not_cens, type = "left")
          ~ dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time),
        weights = ref_wts,
        data = to_mod, 
        dist = 'gaussian'
        )}, silent = TRUE)
      
      # test if model worked
      if('try-error' %in% class(mod)) next
      if(is.nan(mod$scale)) next
        
      # predicted values, log-space
      fits <-predict(
        mod,
        newdata = pred_dat
        )
      names(fits) <- names(fit_grds)
      
      # sd of model at each point in the grid
      sclfits <- mod$scale
      names(sclfits) <- names(scl_grds)

      # predicted values, observed
      btfits <- exp((mod$scale^2)/2)
      btfits <- btfits * exp(fits)
      names(btfits) <- names(bt_grds)
    
      # save output to grids
      fit_ind <- names(fit_grds)
      fit_grds[[fit_ind]][row, i] <- fits[fit_ind]
      bt_ind <- names(bt_grds)
      bt_grds[[bt_ind]][row, i] <- btfits[bt_ind]
      scl_ind <- names(scl_grds)
      scl_grds[[scl_ind]][row, i] <- sclfits[scl_ind]
    
    }
    
  }
  
  # half-window widths for attributes
  ref_wts <- getwts(dat_in, ref_in, wins_only = TRUE, ...)
  
  # add year, month to fit grids
  # expand for full month, year combo 
  fit_grds <- lapply(fit_grds, function(x) {
    fill_grd(x, dat_in)
  })
  
  # add year, month to bt grids
  bt_grds <- lapply(bt_grds, function(x) {
    fill_grd(x, dat_in)
  })
  
  # add year, month to scl grids
  scl_grds <- lapply(scl_grds, function(x) {
    fill_grd(x, dat_in)
  })
  
  # add grids to tidal object, return
  attr(dat_out, 'half_wins') <- ref_wts
  attr(dat_out, 'fits') <- fit_grds
  attr(dat_out, 'bt_fits') <- bt_grds
  attr(dat_out, 'scls') <- scl_grds
  attr(dat_out, 'sal_grd') <- sal_grd
  
  if(trace) cat('\n')
  
  return(dat_out)
  
}

