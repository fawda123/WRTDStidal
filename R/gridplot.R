#' Plot variable response to salinity/flow as a gridded surface for all months
#' 
#' Plot the relationship between the response variable and salinity/flow across the time series using a gridded surface for all months.   The response is shaded by relative values across all dates for comparison.
#' 
#' @param dat_in input tidal or tidalmean object
#' @param month numeric indicating months to plot or chr string 'all' to indicate all months with no plot facets
#' @param tau numeric vector of quantile to plot.  The function will plot the 'middle' quantile if none is specified, e.g., if 0.2, 0.3, and 0.4 are present in the fitted model object then 0.3 will be plotted.
#' @param years numeric vector of years to plot, defaults to all
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}} and \code{\link[ggplot2]{scale_fill_gradientn}} for grid shading.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param logspace logical indicating if plots are in log space
#' @param allflo logical indicating if the salinity/flow values for plotting are limited to the fifth and ninety-fifth percentile of observed values for the month of interest
#' @param interp logical indicating if the response variable between integer years and salinity/flow is linearly interpolated to create a smoother grid 
#' @param flo_fac numeric value indicating the factor for smoothing the response variable across salinity/flow values. Increasing the value creates more smoothing and setting the value to 1 removes all smoothing.
#' @param yr_fac numeric value indicating the factor for smoothing the response variable across integer years. Increasing the value creates more smoothing and setting the value to 1 removes all smoothing.
#' @param ncol numeric argument passed to \code{\link[ggplot2]{facet_wrap}} indicating number of facet columns
#' @param grids logical indicating if grid lines are present
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to other methods
#' 
#' @details These plots can be used to examine how the relationship between the response variable and salinity/flow varies throughout the time series for multiple months.  The plot is similar to that returned by \code{\link{dynaplot}} except changes in the response are shown on a gridded surface of salinity/flow versus time.  Multiple months can also be viewed.  Color shading is in proportion to the value of the response variable and is relative across the plotted months.  The interpolation grid that is stored as an attribute in a fitted tidal object is used to create the plot.  By default, the plots are constrained to the fifth and ninety-fifth percentile of observed salinity/flow values during each month to limit the predictions within the data domain. This behavior can be suppressed by changing the \code{allflo} argument, although the predicted values of the response variable that are outside of the salinity/flow range for the plotted month are typically unrealistic.  
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @seealso \code{\link{dynaplot}}, \code{\link{fitplot}}, \code{\link{gridplot}}, \code{\link{prdnrmplot}}
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' \dontrun{
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' ## defaults to the fiftieth quantile
#' gridplot(tidfit)
#' 
#' ## no facets, all months
#' gridplot(tidfit, month = 'all')
#' 
#' ## change the defaults
#' gridplot(tidfit, tau = c(0.1), month = c(3, 6, 9, 12), 
#'  col_vec = c('red', 'blue', 'green'), flo_fac = 1)
#'  
#' ## plot a tidalmean object
#' data(tidfitmean)
#' 
#' gridplot(tidfitmean)
#' 
#' }
gridplot <- function(dat_in, ...) UseMethod('gridplot')

#' @rdname gridplot
#' 
#' @export 
#' 
#' @method gridplot tidal
gridplot.tidal <- function(dat_in, month = c(1:12), tau = NULL, years = NULL, col_vec = NULL, logspace = TRUE, allflo = FALSE, interp = TRUE, flo_fac = 3, yr_fac = 3, ncol = NULL, grids = FALSE, pretty = TRUE, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert month vector to those present in data
  allmo <- FALSE
  if('all' %in% month){ 
    allmo <- TRUE
    month <- c(1:12)
  }
  
  month <- month[month %in% dat_in$month]
  if(length(month) == 0) stop('No observable data for the chosen month')
  
  # salinity/flow grid values
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
  
  # reshape data frame
  names(to_plo)[grep('^X', names(to_plo))] <- paste('flo', flo_grd)
  to_plo <- tidyr::gather(to_plo, 'flo', 'res', 5:ncol(to_plo)) %>% 
    mutate(flo = as.numeric(gsub('^flo ', '', flo))) %>% 
    select(-date, -day) %>% 
    group_by(year, month, flo) %>% 
    summarize(
      res = mean(res, na.rm = TRUE)
    )
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
     
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
  
  ## use linear interpolation to make a smoother plot
  if(interp & !allmo){
    
    # these are factors by which salinity/flow and years are multiplied for interpolation
    flo_fac <- length(flo_grd) * flo_fac
    flo_fac <- seq(min(flo_grd), max(flo_grd), length.out = flo_fac)
    yr_fac <- length(unique(to_plo$year)) * yr_fac
    yr_fac <- seq(min(to_plo$year), max(to_plo$year), length.out = yr_fac)
    
    # separately by month
    to_plo <- split(to_plo, to_plo$month)

    to_plo <- lapply(to_plo, function(x){
      
      # interp across salinity/flow first
      interped <- lapply(
        split(x, x$year), 

        function(y){
          out <- stats::approx(y$flo, y$res, xout = flo_fac)
          out <- data.frame(year = unique(y$year), month = unique(y$month), out)
          return(out)
        })
      interped <- do.call('rbind', interped)
      names(interped) <- c('year', 'month', 'flo', 'res')
      
      # interp across years
      interped <- lapply(
        split(interped, interped$flo), 
        function(y){
          out <- approx(y$year, y$res, xout = yr_fac)
          out <- data.frame(year = out$x, month = unique(y$month), flo = unique(y$flo), res = out$y)
          return(out)
        })
      interped <- do.call('rbind', interped)
      names(interped) <- c('year', 'month', 'flo', 'res')
    
      return(interped)
    
    })
    
    to_plo <- do.call('rbind', to_plo)
    row.names(to_plo) <- 1:nrow(to_plo)
      
  }
  
  # constrain plots to salinity/flow limits for the selected month
  if(!allflo & !allmo){
    
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
    
    # reduce data
    sel_vec <- with(to_plo, 
      flo >= Low &
      flo <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }

  # change month vector of not plotting all months in same plot
  if(!allmo){
    # months labels as text
    mo_lab <- data.frame(
      num = seq(1:12), 
      txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
    )
    mo_lab <- mo_lab[mo_lab$num %in% month, ]
    to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  } else {
    
    to_plo$year <- with(to_plo, year + (month - 1)/12)
    
  }
    
  # make plot
  p <- ggplot(to_plo, aes(x = year, y = flo, fill = res)) + 
    geom_tile(data = subset(to_plo, !is.na(to_plo$res)), aes(fill = res)) +
    geom_tile(data = subset(to_plo,  is.na(to_plo$res)), fill = 'black', alpha = 0) 

  if(!allmo) p <- p + facet_wrap(~month, ncol = ncol)
  
  # return bare bones if FALSE
  if(!pretty) return(p)
  
  # get colors
  cols <- gradcols(col_vec = col_vec)
  
  p <- p +
    theme_bw() +
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
      )  +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(xlabel, expand = c(0,0)) +
    scale_fill_gradientn(ylabel, colours = rev(cols)) +
    guides(fill = guide_colourbar(barwidth = 10)) 

  # add grid lines
  if(!grids) 
    p <- p + 
      theme(   
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
    
}

#' @rdname gridplot
#' 
#' @export 
#' 
#' @method gridplot tidalmean
gridplot.tidalmean <- function(dat_in, month = c(1:12), years = NULL, col_vec = NULL, logspace = TRUE, allflo = FALSE, interp = TRUE, flo_fac = 3, yr_fac = 3, ncol = NULL, grids = FALSE, pretty = TRUE, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
 
  # convert month vector to those present in data
  allmo <- FALSE
  if('all' %in% month){ 
    allmo <- TRUE
    month <- c(1:12)
  }
   
  # convert month vector to those present in data
  month <- month[month %in% dat_in$month]
  if(length(month) == 0) stop('No observable data for the chosen month')
  
  # salinity/flow grid values
  flo_grd <- attr(dat_in, 'flo_grd')

  # get the selected months
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
  
  # reshape data frame
  to_plo <- to_plo[to_plo$month %in% month, , drop = FALSE]
  names(to_plo)[grep('^X', names(to_plo))] <- paste('flo', flo_grd)
  to_plo <- tidyr::gather(to_plo, 'flo', 'res', 5:ncol(to_plo)) %>% 
    mutate(flo = as.numeric(gsub('^flo ', '', flo))) %>% 
    select(-date, -day) %>% 
    group_by(year, month, flo) %>% 
    summarize(
      res = mean(res, na.rm = TRUE)
    )
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
     
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
  
  ## use linear interpolation to make a smoother plot
  if(interp & !allmo){
    
    # these are factors by which salinity/flow and years are multiplied for interpolation
    flo_fac <- length(flo_grd) * flo_fac
    flo_fac <- seq(min(flo_grd), max(flo_grd), length.out = flo_fac)
    yr_fac <- length(unique(to_plo$year)) * yr_fac
    yr_fac <- seq(min(to_plo$year), max(to_plo$year), length.out = yr_fac)
    
    # separately by month
    to_plo <- split(to_plo, to_plo$month)

    to_plo <- lapply(to_plo, function(x){
      
      # interp across salinity/flow first
      interped <- lapply(
        split(x, x$year), 
        function(y){
          out <- approx(y$flo, y$res, xout = flo_fac)
          out <- data.frame(year = unique(y$year), month = unique(y$month), out)
          return(out)
        })
      interped <- do.call('rbind', interped)
      names(interped) <- c('year', 'month', 'flo', 'res')
      
      # interp across years
      interped <- lapply(
        split(interped, interped$flo), 
        function(y){
          out <- approx(y$year, y$res, xout = yr_fac)
          out <- data.frame(year = out$x, month = unique(y$month), flo = unique(y$flo), res = out$y)
          return(out)
        })
      interped <- do.call('rbind', interped)
      names(interped) <- c('year', 'month', 'flo', 'res')
    
      return(interped)
    
    })
    
    to_plo <- do.call('rbind', to_plo)
    row.names(to_plo) <- 1:nrow(to_plo)
      
  }
  
  # constrain plots to salinity/flow limits for the selected month
  if(!allflo & !allmo){
    
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
    
    # reduce data
    sel_vec <- with(to_plo, 
      flo >= Low &
      flo <= High
      )
    to_plo <- to_plo[sel_vec, !names(to_plo) %in% c('Low', 'High')]
    to_plo <- arrange(to_plo, year, month)
    
  }
  
  # change month vector of not plotting all months in same plot
  if(!allmo){
    # months labels as text
    mo_lab <- data.frame(
      num = seq(1:12), 
      txt = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
    )
    mo_lab <- mo_lab[mo_lab$num %in% month, ]
    to_plo$month <- factor(to_plo$month, levels =  mo_lab$num, labels = mo_lab$txt)
  } else {
    
    to_plo$year <- with(to_plo, year + (month - 1)/12)
    
  }
    
  
  # make plot
  p <- ggplot(to_plo, aes(x = year, y = flo, fill = res)) + 
    geom_tile(data = subset(to_plo, !is.na(to_plo$res)), aes(fill = res)) +
    geom_tile(data = subset(to_plo,  is.na(to_plo$res)), fill = 'black', alpha = 0)
  
  if(!allmo) p <- p + facet_wrap(~month, ncol = ncol)
  
  # return bare bones if FALSE
  if(!pretty) return(p)
  
  # get colors
  cols <- gradcols(col_vec = col_vec)
  
  p <- p +
    theme_bw() +
    theme(
      legend.position = 'top',
      axis.title.x = element_blank()
      )  +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(xlabel, expand = c(0,0)) +
    scale_fill_gradientn(ylabel, colours = rev(cols)) +
    guides(fill = guide_colourbar(barwidth = 10)) 
    
  # add grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
    
}