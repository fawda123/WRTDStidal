#' Plot chlorophyll response to salinity
#' 
#' Plot the relationship between chlorophyll and salinity across the time series.  This can be used to evaluate temporal variation in potential causation between the two.  
#' 
#' @param tidal_in input tidal object
#' @param month numeric input from 1 to 12 indicating the monthly predictions to plot
#' @param tau numeric vector of quantile to plot.  The function will plot the 'middle' quantile if none is specified, e.g., if 0.2, 0.3, and 0.4 are present in the fitted model object then 0.3 will be plotted.
#' @param years numeric vector of years to plot, defaults to all
#' @param logspace logical indicating if plots are in log space
#' @param allsal logical indicating if the salinity values for plotting are limited to the fifth and ninety-fifth percentile of observed salinity values for the month of interest
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to other methods
#' 
#' @details The plot created by this function can be used to examine how the relationship between chlorophyll and salinity varies throughout the time series.  The interpolation grid that is stored as an attribute in a fitted tidal object is used to create the plot.  All predicted chlorophyll values for a selected month across all years are plotted in relation to the range of salinity values that were used to create the interpolation grid.  The plot is limited to the same month throughout the time series to limit seasonal variation.  By default, the plot constrains the salinity values to the fifth and ninety-fifth percentile of observed salinity values during the month of interest to constrain the predictions within the data domain. This behavior can be suppressed by changing the \code{allsal} argument, although the predicted chlorophyll values that are outside of the salinity range for the plotted month are unrealistic.  
#' 
#' Note that the year variable used for color mapping in the plot is treated as a continuous variable although it is an integer by definition.
#' 
#' @import dplyr ggplot2 RColorBrewer
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
#' ## plot using defaults, 
#' ## defaults to the fiftieth quantile for July for all years
#' dynaplot(tidfit)
#' 
#' ## change the defaults
#' dynaplot(tidfit, tau = 0.9, month = 2, years = seq(1980, 1990)) 
dynaplot <- function(tidal_in, ...) UseMethod('dynaplot')

#' @rdname dynaplot
#' 
#' @export 
#' 
#' @method dynaplot tidal
dynaplot.tidal <- function(tidal_in, month = 7, tau = NULL, years = NULL, logspace = FALSE, pretty = TRUE, allsal = FALSE,...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')

  # salinity grid values
  sal_grd <- attr(tidal_in, 'sal_grd')
  
  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_fits <- grep('^fit', names(tidal_in))
    tau_fits <- floor(median(tau_fits))
    tau_fits <- names(tidal_in)[tau_fits]
     
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(tidal_in))) == 0)
      stop('Specified tau not in object')
    
    tau_fits <- paste0('fit', tau)
    
  }

  # get quantile preds to plot and subset by month
  to_plo <- attr(tidal_in, 'fits')[[tau_fits]]
  to_plo <- to_plo[tidal_in$month %in% month, ]
  
  # y-axis label
  ylabel <- expression(
    paste('log-Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
    )

  # back-transform if needed
  if(!logspace){
    
    ylabel <- expression(
      paste('Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
      )
    
    to_plo<- exp(to_plo)

  }
  
  # reshape data frame
  yrs <- tidal_in$year[tidal_in$month %in% month]
  to_plo <- data.frame(year = yrs, to_plo)
  names(to_plo)[grep('^X', names(to_plo))] <- paste('sal', sal_grd)
  to_plo <- tidyr::gather(to_plo, 'sal', 'chla', 2:ncol(to_plo)) %>% 
    mutate(sal = as.numeric(gsub('^sal ', '', sal)))
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
     
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }

  # constrain plots to salinity limits for the selected month
  if(!allsal){
    
    #min, max salinity values to plot
    lim_vals<- group_by(data.frame(tidal_in), month) %>% 
      summarize(
        Low = quantile(sal, 0.05, na.rm = TRUE),
        High = quantile(sal, 0.95, na.rm = TRUE)
      )
  
    # month sal ranges for plot
    lim_vals <- lim_vals[lim_vals$month == month, ]
      
    # reduce data
    sel_vec <- with(to_plo, 
      sal >= lim_vals$Low &
      sal <= lim_vals$High
      )
    to_plo <- to_plo[sel_vec, ]
    
  }
  
  # make plot
  p <- ggplot(to_plo, aes(x = sal, y = chla, group = year)) + 
    geom_line()
  
  # return bare bones if FALSE
  if(!pretty) return(p)
  
  p <- p + 
    geom_line(size = 1, aes(colour = year)) +
    scale_y_continuous(ylabel) +
    scale_x_continuous('Salinity') +
    theme_bw() +
    scale_colour_gradientn('Year', colours = brewer.pal(11, 'Spectral'))
    
  return(p)
    
}