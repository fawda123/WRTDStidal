#' Plot time slices within a tidal object
#' 
#' Plot time slices within a tidal object to view chlorophyll observations, predictions, and normalized results at regular annual intervals.
#' 
#' @param tidal_in input tidal object
#' @param slices numeric vector of calender months to plot, i.e., 1 - 12
#' @param tau numeric vector of quantile to plot, defaults to median quantile if none specified
#' @param predicted logical indicating if standard predicted values are plotted, default \code{TRUE}, otherwise normalized predictions are plotted
#' @param logspace logical indicating if plots are in log space
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @details This is a modification of \code{\link{fitplot}} that can be used to plot selected time slices from the results of a fitted \code{\link{tidal}} object.  For example, all results for a particular month across all years can be viewed.  This is useful for evaluating between-year differences in results for constant season.  Only one quantile fit can be shown per plot because the grouping variable is mapped to the slices.
#' 
#' @import dplyr ggplot2 RColorBrewer tidyr
#' 
#' @export
#' 
#' @seealso \code{\link{fitplot}} 
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
#' # change x-axis labelling
#' library(ggplot2)
#' sliceplot(tidfit) + 
#'  scale_x_continuous(breaks = seq(2000, 2012, by = 4))
#' 
#' # get different months - march and september
#' sliceplot(tidfit, slices = c(3, 9))
#' 
#' # normalized chlorophyll predictions, 10th percentile
#' sliceplot(tidfit, tau = 0.1, predicted = FALSE)
#' 
#' # normalized values all months, change line aesthetics, log-space, 90th 
#' # add title
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
sliceplot <- function(tidal_in, ...) UseMethod('sliceplot')

#' @rdname sliceplot
#' 
#' @export 
#' 
#' @method sliceplot tidal
sliceplot.tidal <- function(tidal_in, slices = c(1, 7), tau = NULL, predicted = TRUE, logspace = FALSE, pretty = TRUE, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')

  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(tidal_in)
  sel_vec <- grepl('^month$|^year$|^chla$|^fit|^norm', names(to_plo))
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
    select(month, year, nrms_variable, nrms_value)
  fits <- gather(to_plo, 'fits_variable', 'fits_value', tau_fits) %>% 
    select(month, year, fits_variable, fits_value)
  
  # y-axis label
  ylabel <- expression(
    paste('log-Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
    )

  # back-transform if needed
  if(!logspace){
    
    ylabel <- expression(
      paste('Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
      )
    
    to_plo$chla <- exp(to_plo$chla)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
  }
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = year, y = chla, group = month)) + 
    geom_point()
      
  # plot fits or nrms
  if(predicted){
    p <- p + 
      geom_line(data = fits, aes(y = fits_value, group = month, 
        colour = month), ...)
    
  } else {
    p <- p + 
      geom_line(data = nrms, aes(y = nrms_value, group = month, 
        colour = month), ...)
    
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
      name = 'Months',
      values = cols
    ) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabel)
  
  return(p)
  
}