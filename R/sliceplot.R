#' Plot time slices within a tidal object
#' 
#' Plot time slices within a tidal object to view chlorophyll observations, predictions, and normalized results at regular annual intervals.
#' 
#' @param dat_in input tidal object
#' @param slices numeric vector of calender months to plot, i.e., 1 - 12
#' @param tau numeric vector of quantile to plot.  The function will plot the 'middle' quantile if none is specified, e.g., if 0.2, 0.3, and 0.4 are present in the fitted model object then 0.3 will be plotted.
#' @param predicted logical indicating if standard predicted values are plotted, default \code{TRUE}, otherwise normalized predictions are plotted
#' @param logspace logical indicating if plots are in log space
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param pt_sz numeric value indicating size of observed chlorophyll points
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @details This is a modification of \code{\link{fitplot}} that can be used to plot selected time slices from the results of a fitted \code{\link{tidal}} object.  For example, all results for a particular month across all years can be viewed.  This is useful for evaluating between-year differences in results for constant season.  Only one quantile fit can be shown per plot because the grouping variable is mapped to the slices.
#' 
#' @import dplyr ggplot2 RColorBrewer tidyr
#' 
#' @export
#' 
#' @seealso \code{\link{fitplot}}, \code{\link{prdnrmplot}}
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' # plot using defaults
#' sliceplot(tidfit)
#' 
#' # get different months - march and september
#' sliceplot(tidfit, slices = c(3, 9))
#' 
#' # normalized chlorophyll predictions, 10th percentile
#' sliceplot(tidfit, tau = 0.1, predicted = FALSE)
#' 
#' # normalized values all months, change line aesthetics, log-space, 90th 
#' # add title
#' library(ggplot2)
#' sliceplot(tidfit, 
#'  slices = 1:12, 
#'  size = 1.5, 
#'  tau = 0.9, 
#'  alpha = 0.6, 
#'  predicted = FALSE, 
#'  logspace = TRUE
#' ) + 
#' ggtitle('Normalized predictions for all months, 90th percentile')
#' 
sliceplot <- function(dat_in, ...) UseMethod('sliceplot')

#' @rdname sliceplot
#' 
#' @export 
#' 
#' @method sliceplot tidal
sliceplot.tidal <- function(dat_in, slices = c(1, 7), tau = NULL, predicted = TRUE, logspace = FALSE, pretty = TRUE, pt_sz = 2, ...){
 
  # sanity check
  if(!is.null(attr(dat_in, 'bt_fits'))) stop('Incorrect input for quantile models')
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
 
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^date$|^month$|^year$|^chla$|^fit|^norm', 
    names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_nrms <- grep('^norm', names(to_plo))
    tau_nrms <- floor(median(tau_nrms))
    tau_fits <- grep('^fit', names(to_plo))
    tau_fits <- floor(median(tau_fits))
     
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(to_plo))) == 0)
      stop('Specified tau not in object')
    
    tau_nrms <- grep(paste0('norm', tau), names(to_plo))
    tau_fits <- grep(paste0('fit', tau), names(to_plo))
    
  }

  # change months values for plotting
  mons <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  to_plo$month <- factor(to_plo$month, levels = seq(1, 12), labels = mons)

  # get months
  to_plo <- to_plo[as.numeric(to_plo$month) %in% slices, ]
  
  # long format for plotting
  nrms <- gather(to_plo, 'nrms_variable', 'nrms_value', tau_nrms) %>% 
    select(date, month, year, nrms_variable, nrms_value)
  fits <- gather(to_plo, 'fits_variable', 'fits_value', tau_fits) %>% 
    select(date, month, year, fits_variable, fits_value)
  
  # y-axis label
  ylabel <- chllab(logspace)
  
  # back-transform if needed
  if(!logspace){
    
    to_plo$chla <- exp(to_plo$chla)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
  }
  
  # quantile on legend title
  quant <- gsub('^fit', '', names(to_plo)[tau_fits])
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = date, y = chla, group = month)) + 
    geom_point(aes(colour = month), size = pt_sz)
     
  # plot fits or nrms
  if(predicted){
    p <- p + 
      geom_line(data = fits, aes(y = fits_value, group = month, 
        colour = month), ...)

    leg_lab <- 'Predicted (lines)\nObserved (pts)'
    
  } else {
    p <- p + 
      geom_line(data = nrms, aes(y = nrms_value, group = month, 
        colour = month), ...)
    
    leg_lab <- 'Normalized (lines)\nObserved (pts)'
  
  }
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  cols <- grDevices::colorRampPalette(
    brewer.pal(10, 'Spectral')
    )(length(slices))

  p <- p + 
    theme_bw() +
    scale_colour_manual(
      name = leg_lab,
      values = cols
    ) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabel)
  
  return(p)
  
}