#' Plot seasonal trends across all years
#' 
#' Plot seasonal trends by combining annual data
#' 
#' @param dat_in Input data object
#' @param tau numeric of quantile to plot
#' @param predicted logical indicating if seasonal smooth is based on model predictions, default \code{TRUE}, otherwise the smooth is based on flow-normalized predictions
#' @param span numeric indicating the smoothing parameter for the loess fit, passed to \code{\link[ggplot2]{stat_smooth}}
#' @param lwd numeric value indicating width of lines
#' @param size numeric value indicating size of points
#' @param alpha numeric value indicating transparency of points or lines
#' @param logspace logical indicating if plots are in log space
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}}.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param grids logical indicating if grid lines are present
#' @param ... arguments passed to other methods
#' 
#' @import dplyr ggplot2
#' 
#' @details Seasonal variation across all years can be viewed by showing the observed annual data on a common y-axis.  The year value is removed from the results such that the y-axis shows only the day of the year.  A simple loess (locally estimated) polynomial smooth is added to show the seasonal trend in the results, where the smoother is fit through the model results for the observed data.  The fit can be smoothed through the model predictions or the flow-normalized predictions, neither of which are shown on the plot.     
#' 
#' @export
#' 
#' @seealso \code{\link{dynaplot}}, \code{\link{fitmoplot}}, \code{\link{gridplot}}, and \code{\link{sliceplot}} produce similar graphics except variation in the same month across years is emphasized.
#' 
#' @examples
#' # load a fitted tidal object
#' data(tidfit)
#' 
#' # plot using defaults
#' # defaults to all quantiles for tidal object
#' seasplot(tidfit)
#' 
#' # tidalmean object
#' seasplot(tidfitmean)
seasplot <- function(dat_in, ...) UseMethod('seasplot')

#' @rdname seasplot
#'
#' @export
#'
#' @method seasplot tidal
seasplot.tidal <- function(dat_in, tau = NULL, predicted = TRUE, span = 0.4, lwd = 1, size = 2, alpha = 1, col_vec = NULL, grids = TRUE, logspace = TRUE, ...){

  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')

  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^date$|^month$|^res$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # add fake date column for annual aggregation in the plot
  to_plo <- mutate(to_plo,
      Year = '1900', 
      mo = as.numeric(strftime(date, '%m')),
      day = as.numeric(strftime(date, '%d')),
      fake_date = as.Date(paste(Year, mo, day, sep = '-'))
    ) 
  
  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_nrms <- grep('^norm', names(to_plo))
    tau_fits <- grep('^fit', names(to_plo))
     
  } else {
   
    if(length(grep(paste0(tau, '$', collapse = '|'), names(to_plo))) == 0)
      stop('Specified tau not in object')
    
    tau_nrms <- grep(paste0('norm', tau, collapse = '|'), names(to_plo))
    tau_fits <- grep(paste0('fit', tau, collapse = '|'), names(to_plo))
    
  }
 
  # long format for plotting
  nrms <- tidyr::gather(to_plo, 'nrms_variable', 'nrms_value', tau_nrms) %>% 
    select(fake_date, nrms_variable, nrms_value)
  fits <- tidyr::gather(to_plo, 'fits_variable', 'fits_value', tau_fits) %>% 
    select(fake_date, fits_variable, fits_value)

  # axis labels
  ylabel <- attr(dat_in, 'reslab')
  xlab <- 'Day of year'
  
  # back-transform if needed
  if(!logspace){

    to_plo$res <- exp(to_plo$res)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
    # strip log, ln  from yaxs label if there
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  
  # formatting for quantile legend labels
  quants <- gsub('^fit', '', names(to_plo)[tau_fits])
  quants <- lapply(as.list(quants), 
    function(x) bquote(italic('\u03c4') ~ .(x))
  )

  # base plot
  p <- ggplot(to_plo, aes_string(x = 'fake_date', y = 'res')) + 
    geom_point(aes(size = 'Observed'), alpha = alpha) + 
    scale_size_manual('', values = size)

  # create plot, fits or nrms
  if(predicted){
    p <- p + 
      geom_smooth(data = fits, aes(y = fits_value, group = fits_variable, 
        colour = fits_variable), size = lwd, alpha = alpha, se = FALSE)
    
    leglab <- c('Predicted\nsmooth')
    
  } else {
    p <- p + 
      geom_smooth(data = nrms, aes(y = nrms_value, group = nrms_variable, 
        colour = nrms_variable), size = lwd, alpha = alpha, se = FALSE)
    
    leglab <- c('Normalized\nsmooth')
  }
  
  # pick colors
  # special case for three quantiles
  colpal <- gradcols(col_vec = col_vec)
  cols <- colpal[round(seq(1, length(colpal), length = length(quants)))]
  if(is.null(col_vec)){
    if(length(quants) == 3) cols <- colpal[c(1, 9, 10)]
    if(length(quants) == 2) cols <- colpal[c(1, 9)]
    if(length(quants) == 1) cols <- colpal[c(1)]
  } 
  
  # change colour scale, format axes
  p <- p + 
    theme_bw() +
    scale_colour_manual(
      name = leglab,
      labels=quants,
      values = cols, 
      guide = guide_legend(reverse = TRUE)
    ) +
    guides(colour = guide_legend(order = 2), size = guide_legend(order = 1)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
      ) +
    scale_y_continuous(ylabel) +
    scale_x_date(xlab, date_labels = "%m/%d")

  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}

#' @rdname seasplot
#'
#' @export
#'
#' @method seasplot tidalmean
seasplot.tidalmean <- function(dat_in, predicted = TRUE, span = 0.4, lwd = 1, size = 2, alpha = 1, col_vec = NULL, grids = TRUE, logspace = TRUE, ...){

  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')

  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^date$|^month$|^res$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # add fake date column for annual aggregation in the plot
  to_plo <- mutate(to_plo,
      Year = '1900', 
      mo = as.numeric(strftime(date, '%m')),
      day = as.numeric(strftime(date, '%d')),
      fake_date = as.Date(paste(Year, mo, day, sep = '-'))
    ) 

  # long format for plotting
  nrms <- tidyr::gather(to_plo, 'nrms_variable', 'nrms_value', norm) %>% 
    select(fake_date, nrms_variable, nrms_value)
  fits <- tidyr::gather(to_plo, 'fits_variable', 'fits_value', fits) %>% 
    select(fake_date, fits_variable, fits_value)

  # axis labels
  ylabel <- attr(dat_in, 'reslab')
  xlab <- 'Day of year'

  # back-transform if needed
  if(!logspace){

    to_plo$res <- exp(to_plo$res)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
    # strip log, ln  from yaxs label if there
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }

  # base plot
  p <- ggplot(to_plo, aes_string(x = 'fake_date', y = 'res')) + 
    geom_point(aes(size = 'Observed'), alpha = alpha) + 
    scale_size_manual('', values = size)
  
  # create plot, fits or nrms
  if(predicted){
    
    p <- p + 
      geom_smooth(data = fits, aes(y = fits_value, group = fits_variable, 
        colour = fits_variable), size = lwd, alpha = alpha, se = FALSE)
    
    leglab <- c('Predicted\nsmooth')
    
  } else {
    
    p <- p + 
      geom_smooth(data = nrms, aes(y = nrms_value, group = nrms_variable, 
        colour = nrms_variable), size = lwd, alpha = alpha, se = FALSE)
    
    leglab <- c('Normalized\nsmooth')
    
  }
  
  # pick colors
  # special case for three quantiles
  cols <- gradcols(col_vec = col_vec)
  
  # change colour scale, format axes
  p <- p + 
    theme_bw() +
    scale_colour_manual(
      labels = leglab,
      values = cols
    ) +
    guides(colour = guide_legend(order = 2), size = guide_legend(order = 1)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
      legend.title = element_blank()
      ) +
    scale_y_continuous(ylabel) +
    scale_x_date(xlab, date_labels = "%m/%d")

  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}
