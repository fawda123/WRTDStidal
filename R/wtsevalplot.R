#' Evaluate window widths for a weighted regression
#'
#' Evaluate a plot of the weighted correlations between chlorophyll and all explanatory variables in the weighted regression for a given set of half-window widths.  AIC values can also be returned.
#'
#' @param dat_in input model object
#' @param wins_in list of input half-window widths of the order months, years, and salinity, passed to \code{\link{getwts}}
#' @param aic logical indicating if AIC is returned instead for the model at each time step
#' @param plot_out logical indicating if a plot output is returned, otherwise a summary for each explanatory variable is 
#' @param trace logical indicating if progress is printed in the console
#' @param ... arguments passed to or from other methods (e.g., \code{\link{getwts}})
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @details  The correlations in the plot are weighted correlations for each point in the time series, where the weights are based on the selected half-window widths in the initial function call.  The total number of correlations in each facet is equal to the total number of observations in the time series.  The weights for each correlation are based on a combined weight vector for the months, years, and salinity values extending from the center of the observation.  The dotted red line shows the average of all correlations in each facet, which can be returned if \code{plot_out = FALSE}.  The AIC value for the model at each observation can also be returned, which may be more informative than simple correlations.   
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object if \code{plot_out = TRUE} showing the a time series correlation of log-chlorophyll with each of the four explanatory variables used in the weighted regression model - salinity, decimal time, and the seasonal components of decimal time.  A vector of summary values are returned if \code{plot_out = FALSE}.
#' 
#' @seealso \code{\link{getwts}}, \code{\link[stats]{cov.wt}}, \code{\link{aiccrq}}, \code{\link{wtsplot}}
#' 
#' @examples
#' 
#' # half-window widths to evaluate
#' # months, years, and salinity
#' wins <- list(0.5, 10, 0.5) 
#' 
#' wtsevalplot(tidobjmean, wins_in = wins)
#' wtsevalplot(tidobjmean, wins_in = wins, aic = TRUE)
#' 
#' # use really small half-window widths
#' # supress behavior of minimum obs in the weight selection
#' wins <- list(0.2, 2, 0.2) 
#' 
#' wtsevalplot(tidobjmean, wins_in = wins, min_obs = FALSE)
#' 
#' # evaluate a quantile model 
#' wtsevalplot(tidobj, wins)
wtsevalplot <- function(dat_in, ...) UseMethod('wtsevalplot')

#' @rdname wtsevalplot
#' 
#' @param tau quantile to evaluate if using quantile regression
#' 
#' @export
#'
#' @method wtsevalplot tidal
wtsevalplot.tidal <- function(dat_in, wins_in, tau = 0.5, aic = FALSE, plot_out = TRUE, trace = TRUE, ...){

  # add sin/cos seasonal components to the input object
  dat_in$sin_dec_time <- sin(2 * pi * dat_in$dec_time)
  dat_in$cos_dec_time <- cos(2 * pi * dat_in$dec_time)

  # variables to correlate, or simply AIC
  to_res <- c('sal', 'dec_time', 'sin_dec_time', 'cos_dec_time')
  if(aic) to_res <- 'AIC'
  
  # progress 
  if(trace){
    txt <- '\nRow-wise estimation, % complete...\n\n'
    cat(txt)
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # moving window correlation 
  res <- matrix(nrow = nrow(tidobj), ncol = length(to_res))
  for(i in 1:nrow(tidobj)){
    
    # progress
    if(trace){
      perc <- 5 * which(i == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    # weights at center of window
    ref_in <- dat_in[i, ]
    ref_wts <- getwts(dat_in, ref_in, wins = wins_in, ...)

    # get the model aic for the center of the window if TRUE
    if(aic){
      
      to_mod <- dat_in[ref_wts > 0, ]
      ref_wts <- ref_wts[ref_wts > 0]
      
      # crq model
      mod <- quantreg::crq(
        Surv(chla, not_cens, type = "left") ~ 
          dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
        weights = ref_wts,
        data = to_mod, 
        method = "Portnoy"
        )
    
      # crq doesn't always work
      test <- try({coef(mod)})
      if('try-error' %in% class(test)) next
      
      # model corrected AIC
      aic <- aiccrq(mod, dat_in, tau)
      parms <- length(coef(mod, tau))
      n <- nrow(to_mod)
      aicc <- aic + (2 * parms * (parms + 1))/(n - parms - 1)
      res[i, ] <- aicc
      
    # otherwise, get the correlations
    } else {
     
       # get correlation of chla with salinity, dec_time given weight vector
      res_out <- cov.wt(dat_in[, c('chla', to_res)], wt = ref_wts, cor = TRUE)
      res_out <- res_out$cor['chla', -1]
    
      res[i, ] <- res_out
      
    }
    
  }
  
  # format correlations for plotting
  res <- data.frame(res)
  names(res) <- to_res
  out <- data.frame(date = dat_in$date, res)
  out <- tidyr::gather(out, variable, value, c(2:ncol(out)))
  out_ave <- aggregate(value ~ variable, data = out, FUN = mean, na.rm = T)
  
  # return the summaries if plot_out is false
  if(!plot_out) return(out_ave)
  
  # the base plot
  p <- ggplot(out, aes(x = date, y = value, group = variable)) + 
    geom_line() + 
    scale_x_date('Date') + 
    geom_hline(data = out_ave, aes(yintercept = value), linetype = 'dashed', colour = 'red') +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw()
  
  # for aic plot
  if(aic){
  
    p <- p + scale_y_continuous('AIC by observation')
  
  # for correlation plot
  } else {
    
    # facet labeller
    fac_look <- list('sal' = 'Salinity', 'dec_time' = 'Decimal time (t)',
      'sin_dec_time' = expression(paste(sin, '(2', pi, 't)')),
      'cos_dec_time' = expression(paste(cos, '(2', pi, 't)'))
    )
    fac_labs <- function(variable, value){
      return(fac_look[value])
    }

    p <- p + 
      scale_y_continuous('Weighted correlations') +
      facet_grid(variable ~ ., labeller = fac_labs)
    
  }
  
  return(p)
  
}

#' @rdname wtsevalplot
#' 
#' @export
#'
#' @method wtsevalplot tidalmean
wtsevalplot.tidalmean <- function(dat_in, wins_in, aic = FALSE, plot_out = TRUE, trace = TRUE, ...){

  # add sin/cos seasonal components to the input object
  dat_in$sin_dec_time <- sin(2 * pi * dat_in$dec_time)
  dat_in$cos_dec_time <- cos(2 * pi * dat_in$dec_time)

  # variables to correlate, or simply AIC
  to_res <- c('sal', 'dec_time', 'sin_dec_time', 'cos_dec_time')
  if(aic) to_res <- 'AIC'
  
  # progress 
  if(trace){
    txt <- '\nRow-wise estimation, % complete...\n\n'
    cat(txt)
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # moving window correlation 
  res <- matrix(nrow = nrow(tidobj), ncol = length(to_res))
  for(i in 1:nrow(tidobj)){
    
    # progress
    if(trace){
      perc <- 5 * which(i == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    # weights at center of window
    ref_in <- dat_in[i, ]
    ref_wts <- getwts(dat_in, ref_in, wins = wins_in, ...)

    # get the model aic for the center of the window if TRUE
    if(aic){
      
      to_mod <- dat_in[ref_wts > 0, ]
      ref_wts <- ref_wts[ref_wts > 0]

      # parametric survival mod
      mod <- try({survival::survreg(
        survival::Surv(chla, not_cens, type = "left")
          ~ dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time),
        weights = ref_wts,
        data = to_mod, 
        dist = 'gaussian', 
        control = survival::survreg.control(iter.max = 200)
        )})
      
      # test if model worked
      test <- try({coef(mod)})
      if('try-error' %in% c(class(mod), class(test))) next
      
      # model corrected AIC
      aic <- AIC(mod)
      parms <- length(coef(mod))
      n <- nrow(to_mod)
      aicc <- aic + (2 * parms * (parms + 1))/(n - parms - 1)
      res[i, ] <- aicc
      
    # otherwise, get the correlations
    } else {
     
       # get correlation of chla with salinity, dec_time given weight vector
      res_out <- cov.wt(dat_in[, c('chla', to_res)], wt = ref_wts, cor = TRUE)
      res_out <- res_out$cor['chla', -1]
    
      res[i, ] <- res_out
      
    }
    
  }
  
  # format correlations for plotting
  res <- data.frame(res)
  names(res) <- to_res
  out <- data.frame(date = dat_in$date, res)
  out <- tidyr::gather(out, variable, value, c(2:ncol(out)))
  out_ave <- aggregate(value ~ variable, data = out, FUN = median, na.rm = T)
  
  # return the summaries if plot_out is false
  if(!plot_out) return(out_ave)
  
  # the base plot
  p <- ggplot(out, aes(x = date, y = value, group = variable)) + 
    geom_line() + 
    scale_x_date('Date') + 
    geom_hline(data = out_ave, aes(yintercept = value), linetype = 'dashed', colour = 'red') +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_bw()
  
  # for aic plot
  if(aic){
  
    p <- p + scale_y_continuous('AIC by observation')
  
  # for correlation plot
  } else {
    
    # facet labeller
    fac_look <- list('sal' = 'Salinity', 'dec_time' = 'Decimal time (t)',
      'sin_dec_time' = expression(paste(sin, '(2', pi, 't)')),
      'cos_dec_time' = expression(paste(cos, '(2', pi, 't)'))
    )
    fac_labs <- function(variable, value){
      return(fac_look[value])
    }

    p <- p + 
      scale_y_continuous('Weighted correlations') +
      facet_grid(variable ~ ., labeller = fac_labs)
    
  }
  
  return(p)
  
}
