#' Plot model response to salinity or flow as a lineplot for all months
#' 
#' Plot the relationship between the modelled response and salinity/flow across the time series using line plots for each month.  Each line corresponds to a unique year.  This can be used to evaluate temporal variation between the two.  
#' 
#' @param dat_in input tidal or tidalmean object
#' @param month numeric input from 1 to 12 indicating the monthly predictions to plot
#' @param tau numeric vector of quantile to plot.  The function will plot the 'middle' quantile if none is specified, e.g., if 0.2, 0.3, and 0.4 are present in the fitted model object then 0.3 will be plotted.
#' @param years numeric vector of years to plot, one to many, defaults to all
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}} and \code{\link[ggplot2]{scale_colour_gradientn}} for line shading.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param alpha numeric value from zero to one indicating line transparency
#' @param size numeric value for line size
#' @param logspace logical indicating if plots are in log space
#' @param floscl logical indicating if salinity/flow on x-axis is standardized (default) or in original scale
#' @param allflo logical indicating if the salinity or flow values for plotting are limited to the fifth and ninety-fifth percentile of observed values for the month of interest
#' @param ncol numeric argument passed to \code{\link[ggplot2]{facet_wrap}} indicating number of facet columns
#' @param grids logical indicating if grid lines are present
#' @param scales chr string passed to ggplot to change x/y axis scaling on facets, acceptable values are \code{'free'}, \code{'free_x'}, or \code{'free_y'}
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used.  The aesthetic arguments will not apply if \code{pretty = TRUE}.
#' @param use_bw logical indicating if the \code{\link[ggplot2]{theme_bw}} theme is used 
#' @param fac_nms optional chr string for facet labels, which must be equal in length to \code{month}
#' @param ... arguments passed to other methods
#' 
#' @details These plots can be used to examine how the relationship between the response variable and flow varies throughout the time series.  It is essentially identical to the plot produced by \code{\link{gridplot}}, except lines plots are returned that show the relationship of the response variable with salinity/flow using different lines for each year. The interpolation grid that is stored as an attribute in a fitted tidal object is used to create the plot.  Each plot is limited to the same month throughout the time series to limit seasonal variation.  Plots are also constrained to the fifth and ninety-fifth percentile of observed salinity/flow values during the month of interest to limit the predictions within the data domain. This behavior can be suppressed by changing the \code{allflo} argument. 
#' 
#' Note that the year variable used for color mapping is treated as a continuous variable although it is an integer by definition.
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @seealso \code{\link{fitplot}}, \code{\link{gridplot}}, \code{\link{prdnrmplot}}
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' 
#' # load a fitted tidal object
#' data(tidfit)
#' 
#' # plot using defaults, 
#' # defaults to the fiftieth quantile for all years
#' dynaplot(tidfit)
#' \dontrun{
#' # change the defaults
#' dynaplot(tidfit, tau = 0.9, month = 2, years = seq(1980, 1990), 
#'  col_vec = rainbow(7), alpha = 0.5, size = 3) 
#'  
#' # plot a tidalmean object
#' data(tidfitmean)
#' 
#' dynaplot(tidfitmean)
#' }
dynaplot <- function(dat_in, ...) UseMethod('dynaplot')

#' @rdname dynaplot
#' 
#' @export 
#' 
#' @method dynaplot tidal
dynaplot.tidal <- function(dat_in, month = c(1:12), tau = NULL, years = NULL, col_vec = NULL, alpha = 1, size = 1, logspace = TRUE, floscl = TRUE, allflo = FALSE, ncol = NULL, grids = TRUE, scales = NULL, pretty = TRUE, use_bw = TRUE, fac_nms = NULL, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')

  # convert month vector to those present in data
  month <- month[month %in% dat_in$month]
  if(length(month) == 0) stop('No observable data for the chosen month')
  
  # flow grid values
  flo_grd <- attr(dat_in, 'flo_grd')

  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_fits <- grep('^fit', names(dat_in))
    tau_fits <- floor(median(tau_fits))
    tau_fits <- names(dat_in)[tau_fits]
     
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(dat_in))) == 0)
      stop('Specified tau not in object')
    
    tau_fits <- paste0('fit', tau)
    
  }

  # get the selected months
  to_plo <- attr(dat_in, 'fits')[[tau_fits]]
  to_plo <- to_plo[to_plo$month %in% month, , drop = FALSE]
  
  # axis labels
  ylabel <- attr(dat_in, 'reslab')
  xlabel <- attr(dat_in, 'flolab')

  # back-transform if needed
  if(!logspace){
  
    to_plo[, -c(1:4)] <- exp(to_plo[, -c(1:4)])
    
    # strip log, ln  from yaxs label if there
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))

  }
  
  # reshape data frame, take year/month average for symmetry
  names(to_plo)[grep('^X', names(to_plo))] <- paste('flo', flo_grd)
  to_plo <- tidyr::gather(to_plo, 'flo', 'res', 5:ncol(to_plo)) %>% 
    mutate(flo = as.numeric(gsub('^flo ', '', flo))) %>% 
    select(-date, -day) %>% 
    group_by(year, month, flo) %>% 
    summarize(
      res = mean(res, na.rm = TRUE)
    )
  
  # change flo to original scale
  if(!floscl){
   
    # grid data
    floobs_rng <- attr(dat_in, 'floobs_rng') 
    floscl_rng <- range(to_plo$flo, na.rm = TRUE)
    to_plo$flo <- (to_plo$flo - floscl_rng[1]) / diff(floscl_rng) * diff(floobs_rng) + floobs_rng[1]
    
    #input data
    floscl_rng <- range(dat_in$flo, na.rm = TRUE)
    dat_in$flo <- (dat_in$flo - floscl_rng[1]) / diff(floscl_rng) * diff(floobs_rng) + floobs_rng[1]
    
  }
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
  
  # constrain plots to salinity/flow limits for the selected month
  if(!allflo){
    
    #min, max salinity/flow values to plot
    lim_vals<- group_by(data.frame(dat_in), month) %>% 
      summarize(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
  
    # month flo ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]

    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
    
    # reduce data
    sel_vec <- with(to_plo, 
      flo >= Low &
      flo <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # reassign facet names if fac_nms is provided
  if(!is.null(fac_nms)){
    
    if(length(fac_nms) != length(unique(to_plo$month))) stop('fac_nms must have same lengths as months')
  
    to_plo$month <- factor(to_plo$month, labels = fac_nms)
    
  }
  
  # make plot
  p <- ggplot(to_plo, aes(x = flo, y = res, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # get colors
  cols <- gradcols(col_vec = col_vec)
  
  # use bw theme
  if(use_bw) p <- p + theme_bw()
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(ylabel, expand = c(0, 0)) +
    scale_x_continuous(xlabel, expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
    
}

#' @rdname dynaplot
#' 
#' @export 
#' 
#' @method dynaplot tidalmean
dynaplot.tidalmean <- function(dat_in, month = c(1:12), years = NULL, col_vec = NULL, alpha = 1, size = 1, logspace = TRUE, floscl = TRUE, allflo = FALSE, ncol = NULL, grids = TRUE, scales = NULL, pretty = TRUE, use_bw = TRUE, fac_nms = NULL, ...){
  
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert month vector to those present in data
  month <- month[month %in% dat_in$month]
  if(length(month) == 0) stop('No observable data for the chosen month')
  
  # salinity/flow grid values
  flo_grd <- attr(dat_in, 'flo_grd')

  # get the grid
  to_plo <- attr(dat_in, 'fits')[[1]]

  # axis labels
  ylabel <- attr(dat_in, 'reslab')
  xlabel <- attr(dat_in, 'flolab')

  # use bt grid if not log-space
  if(!logspace){
    
    to_plo <- attr(dat_in, 'bt_fits')[[1]]
   
    # strip log, ln  from yaxs label if there
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  
  # reshape data frame, average by year, month for symmetry
  to_plo <- to_plo[to_plo$month %in% month, , drop = FALSE]
  names(to_plo)[grep('^X', names(to_plo))] <- paste('flo', flo_grd)
  to_plo <- tidyr::gather(to_plo, 'flo', 'res', 5:ncol(to_plo)) %>% 
    mutate(flo = as.numeric(gsub('^flo ', '', flo))) %>% 
    select(-date, -day) %>% 
    group_by(year, month, flo) %>% 
    summarize(
      res = mean(res, na.rm = TRUE)
    )
  
  # change flo to original scale
  if(!floscl){
   
    # grid data
    floobs_rng <- attr(dat_in, 'floobs_rng') 
    floscl_rng <- range(to_plo$flo, na.rm = TRUE)
    to_plo$flo <- (to_plo$flo - floscl_rng[1]) / diff(floscl_rng) * diff(floobs_rng) + floobs_rng[1]
    
    #input data
    floscl_rng <- range(dat_in$flo, na.rm = TRUE)
    dat_in$flo <- (dat_in$flo - floscl_rng[1]) / diff(floscl_rng) * diff(floobs_rng) + floobs_rng[1]
    
  }
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
    to_plo <- to_plo[to_plo$month %in% month, ]
        
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
  
  # constrain plots to salinity/flow limits for the selected month
  if(!allflo){
    
    #min, max flow values to plot
    lim_vals<- group_by(data.frame(dat_in), month) %>% 
      summarize(
        Low = quantile(flo, 0.05, na.rm = TRUE),
        High = quantile(flo, 0.95, na.rm = TRUE)
      )
  
    # month flo ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]

    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    to_plo <- to_plo[to_plo$month %in% month, ]
        
    # reduce data
    sel_vec <- with(to_plo, 
      flo >= Low &
      flo <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # months labels as text
  mo_lab <- data.frame(
    num = seq(1:12), 
    txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
  )
  mo_lab <- mo_lab[mo_lab$num %in% month, ]
  to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  
  # reassign facet names if fac_nms is provided
  if(!is.null(fac_nms)){
    
    if(length(fac_nms) != length(unique(to_plo$month))) stop('fac_nms must have same lengths as months')
  
    to_plo$month <- factor(to_plo$month, labels = fac_nms)
    
  }
  
  # make plot
  p <- ggplot(to_plo, aes(x = flo, y = res, group = year)) + 
    facet_wrap(~month, ncol = ncol, scales = scales)
  
  # return bare bones if FALSE
  if(!pretty) return(p + geom_line())
  
  # get colors
  cols <- gradcols(col_vec = col_vec)
  
  # use bw theme
  if(use_bw) p <- p + theme_bw()
  
  p <- p + 
    geom_line(size = size, aes(colour = year), alpha = alpha) +
    scale_y_continuous(ylabel, expand = c(0, 0)) +
    scale_x_continuous(xlabel, expand = c(0, 0)) +
    theme(
      legend.position = 'top'
    ) +
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) 
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
    
}