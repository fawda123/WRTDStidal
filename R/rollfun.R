#' Function passed to \code{\link[zoo]{rollapply}} in \code{\link{wrtds2}}
#'
#' @param x row
#' @param dat_in input data frame that is rolled
#'
wrtdsroll <- function(x, dat_in){
    
    ref_in <- x
    
#     # progress
#     if(trace){
#       perc <- 5 * which(x == counts)
#       if(length(perc) != 0) cat(perc, '\t')
#     }
#     
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
      mod <- survival::survreg(
        survival::Surv(chla, not_cens, type = "left")
          ~ dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time),
        weights = ref_wts,
        data = to_mod, 
        dist = 'gaussian', 
        control = survival::survreg.control(iter.max = 200)
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
      
      # sd of model at each point in the grid
      sclfits <- mod$scale
      names(sclfits) <- names(scl_grds)

      # predicted values, observed
      btfits <- exp((mod$scale^2)/2)
      btfits <- btfits * exp(fits)
      names(btfits) <- names(bt_grds)
    
      return(fits, sclfits, btfits)
#       # save output to grids
#       fit_ind <- names(fit_grds)
#       fit_grds[[fit_ind]][row, i] <- fits[fit_ind]
#       bt_ind <- names(bt_grds)
#       bt_grds[[bt_ind]][row, i] <- btfits[bt_ind]
#       scl_ind <- names(scl_grds)
#       scl_grds[[scl_ind]][row, i] <- sclfits[scl_ind]
    
    }
    
  }