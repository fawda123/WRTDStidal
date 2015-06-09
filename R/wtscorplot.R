data(tidobj)

mos <- c(seq(0.5, 1, by = 0.25), 2, 10)
yrs <- c(seq(5, 15, by = 3), 50)
sal <- c(seq(0.5, 1, by = 0.1), 5)
grd <- expand.grid(mos, yrs, sal)
names(grd) <- c('mos', 'yrs', 'sal')

wins_in <- grd[1, ]
dat_in <- tidobj

#' Evaluate weighted correlation of chlorophyll with model explanatory variables
#'
#' Evaluated a plot of the weighted correlation between chlorophyll and all explanatory variables in the weighted regression for a given set of half-window widths.
#'
#' @param dat_in input model object
#' @param wins_in list of input half-window widths of the order months, years, and salinity, passed to \code{\link{getwts}}
#' @param plot_out logical indicating if a plot output is returned, otherwise a summary for each explanatory variable is 
#' @param trace logical indicating if progress is printed in the console
#' @param ... arguments passed to or from other methods (e.g., \code{\link{getwts}})
#' 
#' @export
#' 
#' @import ggplot2
#' 
#' @details  The correlations in the plot are weighted correlations for each point in the time series, where the weights are based on the selected half-window widths in the initial function call.  The total number of correlations in each facet is equal to the total number of observations in the time series.  The weights for each correlation are based on a combined weight vector for the months, years, and salinity values extending from the center of the observation.  The dotted red line shows the average of all correlations in each facet, which can be returned if \code{plot_out = FALSE}.  
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object if \code{plot_out = TRUE} showing the a time series correlation of log-chlorophyll with each of the four explanatory variables used in the weighted regression model - salinity, decimal time, and the seasonal components of decimal time.  A vector of summary values are returned if \code{plot_out = FALSE}.
#' 
#' @seealso \code{\link{getwts}}, \code{\link[stats]{cov.wt}}
#' 
#' @examples
#'
#' data(tidobj)
#' 
#' # half-window widths to evaluate
#' # months, years, and salinity
#' wins <- list(0.5, 10, 0.5) 
#' 
#' wtscorplot(tidobj, wins_in = wins)
#' 
#' # use really small half-window widths
#' # supress behavior of minimum obs in the weight selection
#' wins <- list(0.2, 2, 0.2) 
#' 
#' wtscorplot(tidobj, wins_in = wins, min_obs = FALSE)
wtscorplot <- function(dat_in, ...) UseMethod('wtscorplot')

#' @rdname wtscorplot
#' 
#' @export
#'
#' @method wtscorplot default
wtscorplot.default <- function(dat_in, wins_in, plot_out = TRUE, trace = TRUE, ...){

  # add sin/cos seasonal components to the input object
  dat_in$sin_dec_time <- sin(2 * pi * dat_in$dec_time)
  dat_in$cos_dec_time <- cos(2 * pi * dat_in$dec_time)

  # variables to correlate
  to_cor <- c('chla', 'sal', 'dec_time', 'sin_dec_time', 'cos_dec_time')

  # progress 
  if(trace){
    txt <- '\nRow-wise estimation, % complete...\n\n'
    cat(txt)
    
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # moving window correlation 
  cors <- matrix(nrow = nrow(tidobj), ncol = 4)
  for(i in 1:nrow(tidobj)){
    
    # progress
    if(trace){
      perc <- 5 * which(i == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
    
    ref_in <- dat_in[i, ]
  
    ref_wts <- getwts(dat_in, ref_in, wins = wins_in, ...)
        
    # get correlation of chla with salinity, dec_time given weight vector
    cor_out <- cov.wt(dat_in[ to_cor], wt = ref_wts, cor = TRUE)
    cor_out <- cor_out$cor['chla', -1]
    
    cors[i, ] <- cor_out
    
  }
  
  # format correlations for plotting
  cors <- data.frame(cors)
  names(cors) <- to_cor[-1]
  out <- data.frame(date = dat_in$date, cors)
  out <- tidyr::gather(out, variable, value, c(2:ncol(out)))
  out_ave <- aggregate(value ~ variable, data = out, FUN = mean, na.rm = T)
  
  # return the summaries if plot_out is false
  if(!plot_out) return(out_ave)
  
  # facet labeller
  fac_look <- list('sal' = 'Salinity', 'dec_time' = 'Decimal time (t)',
    'sin_dec_time' = expression(paste(sin, '(2', pi, 't)')),
    'cos_dec_time' = expression(paste(cos, '(2', pi, 't)'))
  )
  fac_labs <- function(variable, value){
    return(fac_look[value])
  }

  # the plot
  p <- ggplot(out, aes(x = date, y = value, group = variable)) + 
    geom_line() + 
    scale_y_continuous('Weighted correlation') + 
    scale_x_date('Date') + 
    geom_hline(data = out_ave, aes(yintercept = value), linetype = 'dashed', colour = 'red') +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    facet_grid(variable ~ ., labeller = fac_labs) +
    theme_bw()
  
  return(p)
  
}