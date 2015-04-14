#' Plot chlorophyll response to salinity as a gridded surface
#' 
#' Plot the relationship between chlorophyll and salinity across the time series using a gridded surface.  This can be used to evaluate temporal variation between the two.  
#' 
#' @param tidal_in input tidal object
#' @param month numeric input from 1 to 12 indicating the monthly predictions to plot
#' @param tau numeric vector of quantile to plot.  The function will plot the 'middle' quantile if none is specified, e.g., if 0.2, 0.3, and 0.4 are present in the fitted model object then 0.3 will be plotted.
#' @param years numeric vector of years to plot, defaults to all
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}} and \code{\link[ggplot2]{scale_fill_gradientn}} for grid shading.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param logspace logical indicating if plots are in log space
#' @param allsal logical indicating if the salinity values for plotting are limited to the fifth and ninety-fifth percentile of observed salinity values for the month of interest
#' @param interp logical indicating if chlorophyll between integer years and salinity is linearly interpolated to create a smoother grid 
#' @param sal_fac numeric value indicating the factor for smoothing chlorophyll across salinity values. Increasing the value creates more smoothing and setting the value to 1 removes all smoothing.
#' @param yr_fac numeric value indicating the factor for smoothing chlorophyll across integer years. Increasing the value creates more smoothing and setting the value to 1 removes all smoothing.
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to other methods
#' 
#' @details The plot can be used to examine how the relationship between chlorophyll and salinity varies throughout the time series.  It is essentially identical to the plot produced by \code{\link{dynaplot}}, except a gridded plot is returned that shows salinity over time with cells colored by chlorophyll.  The interpolation grid that is stored as an attribute in a fitted tidal object is used to create the plot.  The plot is limited to the same month throughout the time series to limit seasonal variation.  By default, the plot is constrained to the fifth and ninety-fifth percentile of observed salinity values during the month of interest to limit the predictions within the data domain. This behavior can be suppressed by changing the \code{allsal} argument, although the predicted chlorophyll values that are outside of the salinity range for the plotted month are typically unrealistic.  
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @seealso \code{\link{dynaplot}}, \code{\link{fitplot}}, \code{\link{prdnrmplot}}
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
#' gridplot(tidfit)
#' 
#' ## change the defaults
#' gridplot(tidfit, tau = 0.9, month = 2, years = seq(1980, 1990), 
#'  sal_fac = 20, col_vec = 'Dark2') 
gridplot <- function(tidal_in, ...) UseMethod('gridplot')

#' @rdname gridplot
#' 
#' @export 
#' 
#' @method gridplot tidal
gridplot.tidal <- function(tidal_in, month = 7, tau = NULL, years = NULL, col_vec = NULL, logspace = FALSE, allsal = FALSE, interp = TRUE, sal_fac = 5, yr_fac = sal_fac, pretty = TRUE, ...){
 
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
  
  ## use linear interpolation to make a smoother plot
  if(interp){
    
    # interp across salinity first
    exp_val <- length(unique(to_plo$sal)) * sal_fac
    interped <- lapply(
      split(to_plo, to_plo$year), 
      function(x){
        out <- approx(x$sal, x$chla, n = exp_val)
        out <- data.frame(year = x$year, out)
        return(out)
      })
    interped <- do.call('rbind', interped)
    names(interped) <- c('year', 'sal', 'chla')
    
    # interp across years (increasd by factor of five times no. of years)
    exp_val <- length(unique(interped$year)) * yr_fac
    interped <- lapply(
      split(interped, interped$sal), 
      function(x){
        out <- approx(x$year, x$chla, n = exp_val)
        out <- data.frame(year = out$x, sal = unique(x$sal), chla = out$y)
        return(out)
      })
    interped <- do.call('rbind', interped)
    names(interped) <- c('year', 'sal', 'chla')
    
    to_plo <- interped
    
  }
  
  # make plot
  p <- ggplot(to_plo, aes(x = year, y = sal, fill = chla)) + 
    geom_tile()
  
  # return bare bones if FALSE
  if(!pretty) return(p)
  
  # get colors
  cols <- gradcols(col_vec = col_vec)
  
  p <- p + 
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
      )  +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous('Salinity', expand = c(0,0)) +
    scale_fill_gradientn(ylabel, colours = rev(cols))
    
  return(p)
    
}