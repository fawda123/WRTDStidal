######
#' Get WRTDS prediction grid
#'
#' Get WRTDS prediction grid for chlorophyll observations in a tidal object
#'
#' @param tidal_in input tidal object
#' @param sal_div numeric indicating number of divisions across the range of salinity as fraction of freshwater to create interpolation grid
#' @param ... arguments passed to other methods
#' 
#' @export
#' 
#' @return Appends a regression grid as an attribute to a tidal object
#' 
#' @examples
#' ## load data
#' data(chldat)
#' 
#' ## get wrtds grid
#' res <- wrtds(chldat)
wrtds <- function(tidal_in, sal_div = 20, ...) UseMethod('wrtds')

#' @rdname wrtds
#'
#' @export
#'
#' @method wrtds data.frame
wrtds.data.frame <- function(tidal_in, sal_div = 20, ...){
  
  dat <- tidal(tidal_in, ...)
  wrtds(dat, sal_div)
  
}

#' @rdname wrtds
#' 
#' @import quantreg
#'
#' @method wrtds tidal
wrtds.tidal <- function(tidal_in, sal_div = 20){

  #salinity values to estimate
  sal_grid <- seq(
    min(tidal_in$sal, na.rm = T), 
    max(tidal_in$sal_ref, na.rm = T), 
    length = sal_div
    )

  seg_out <- matrix(nrow = nrow(tidal_in), ncol = sal_div)
  
  for(row in 1:nrow(tidal_in)){
    
    row_out <- NULL
    
    ref_in <- tidal_in[row, ]
    
    cat(as.character(seg), nrow(tidal_in) - row, '\n')

    for(sal in sal_grid){
      
      ref_in$sal_ref <- sal
      ref_wts <- getwts(ref_in, tidal_in)
      
      # data to predict
      pred_dat <- data.frame(sal_ref = sal, dec_time = ref_in$dec_time)
      
      # crq model, estimates all quants
      mod <- quantreg::crq(
        Surv(Chla_ugl, not_cens, type = "left") ~ 
          dec_time + sal_ref + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
        weights = ref_wts,
        data = tidal_in, 
        method = "Portnoy"
        )
      
      # sometimes crq fucks up
      test <- try({coef(mod)})
      if('try-error' %in% class(test)){
        err_out <- rep(NA, 9)
        row_out <- rbind(row_out, c(err_out))
        next
      }
        
      # fitted coefficients for each model
      parms <- coef(mod, c(0.1, 0.5, 0.9))
      
      # predicted values by quantile model coefficients
      fits <- sapply(1:3, function(x){
        with(pred_dat, 
          parms[1, x] + parms[2, x] * dec_time + parms[3, x] * sal_ref + parms[4, x] * sin(2*pi*dec_time) + parms[5, x] * cos(2*pi*dec_time)
        )
      })
      names(fits) <- paste0('fit_', c('lo', 'md', 'hi'))
      
      # back transformed predicted values
      bt_fits <- exp(fits)
      names(bt_fits) <- paste0('bt_', c('lo', 'md', 'hi'))
      
      # model parameters for sal_ref
      betas <- coef(mod, c(0.1, 0.5, 0.9))['sal_ref', ]
      names(betas) <- paste0('b_', c('lo', 'md', 'hi'))
      
      #append to row out for each unique sal
      row_out <- rbind(
        row_out,
        c(fits, bt_fits, betas)
        )
      
      }
    
    wt_fits <- suppressWarnings(data.frame(
      year = ref_in$year,
      month_num = ref_in$month_num,
      date_f = ref_in$date_f,
      dec_time = ref_in$dec_time,
      seg,
      sal_grid,
      row_out
      ))
    
    seg_out <- rbind(seg_out,wt_fits)
    
    }
  
  seg_out
  
}
