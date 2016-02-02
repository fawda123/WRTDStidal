#' Plot number of observations in a WRTDS interpolation grid
#' 
#' Plot number of observations for each point in a WRTDS interpolation grid.  This is a diagnostic plot to identify sample size for each unique location in the domain of the time series that is considered during model fitting.  
#' 
#' @param dat_in input tidal or tidalmean object
#' @param month numeric indicating months to plot or chr string 'all' to indicate all months with no plot facets
#' @param years numeric vector of years to plot, defaults to all
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}} and \code{\link[ggplot2]{scale_fill_gradientn}} for grid shading.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param allsal logical indicating if the salinity values for plotting are limited to the fifth and ninety-fifth percentile of observed salinity values for the month of interest
#' @param ncol numeric argument passed to \code{\link[ggplot2]{facet_wrap}} indicating number of facet columns
#' @param grids logical indicating if grid lines are present
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to other methods
#' 
#' @details The plots can be used sample size as an indication of model fit for each unique location in the domain space of the time series.  The plots show grids of the number of observations with weights greater than zero for each unique date and salinity combination.  The \code{obs} attribute in the \code{tidal} or \code{tidalmean} object is created during model fitting and has the same dimensions as the interpolation grid.  Each row is a unique date in the original dataset and each column is a salinity value used to fit each regression (i.e., values in the \code{sal_grd} attribute). In general, low points in the grid may indicate locations in the time series where insufficient data could affect model fit.
#' 
#' Unlike \code{\link{gridplot}}, interpolation of the grids for a smoother appearance is not allowed because the objecive is to identify specific locations with low sample size.  For the former function, the objective is to characterize general trends over time rather values at specific locations.  
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @seealso \code{\link{wtsplot}} for an alternative to evaluating weights with different window width combinations
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' \dontrun{
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' ## default plot
#' nobsplot(tidfit)
#' 
#' ## no facets, all months
#' nobsplot(tidfit)
#' 
#' ## change the defaults
#' nobsplot(tidfit, tau = c(0.1), month = c(3, 6, 9, 12), 
#'  col_vec = c('red', 'blue', 'green'), sal_fac = 1)
#'  
#' ## plot a tidalmean object
#' data(tidfitmean)
#' 
#' nobsplot(tidfitmean)
#' 
#' }
nobsplot <- function(dat_in, ...) UseMethod('nobsplot')

#' @rdname nobsplot
#' 
#' @export 
#' 
#' @method nobsplot default
nobsplot.default <- function(dat_in, month = 'all', years = NULL, col_vec = NULL, allsal = TRUE, ncol = NULL, grids = FALSE, pretty = TRUE, ...){
 
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
  
  # salinity grid values
  sal_grd <- attr(dat_in, 'sal_grd')

  # get the selected months
  to_plo <- attr(dat_in, 'nobs')[[1]]
  to_plo <- to_plo[to_plo$month %in% month, , drop = FALSE]
  
  # axis labels
  ylabel <- 'Observations'
  xlabel <- attr(dat_in, 'flolab')

  # reshape data frame
  names(to_plo)[grep('^X', names(to_plo))] <- paste('sal', sal_grd)
  to_plo <- tidyr::gather(to_plo, 'sal', 'nobs', 5:ncol(to_plo)) %>% 
    mutate(sal = as.numeric(gsub('^sal ', '', sal))) %>% 
    select(-date, -day)
  
  # subset years to plot
  if(!is.null(years)){
    
    to_plo <- to_plo[to_plo$year %in% years, ]
     
    if(nrow(to_plo) == 0) stop('No data to plot for the date range')
  
  }
  
  # constrain plots to salinity limits for the selected month
  if(!allsal & !allmo){
    
    #min, max salinity values to plot
    lim_vals<- group_by(data.frame(dat_in), month) %>% 
      summarize(
        Low = quantile(sal, 0.05, na.rm = TRUE),
        High = quantile(sal, 0.95, na.rm = TRUE)
      )
  
    # month sal ranges for plot
    lim_vals <- lim_vals[lim_vals$month %in% month, ]
    
    # merge limts with months
    to_plo <- left_join(to_plo, lim_vals, by = 'month')
    
    # reduce data
    sel_vec <- with(to_plo, 
      sal >= Low &
      sal <= High
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
  p <- ggplot(to_plo, aes(x = year, y = sal, fill = nobs)) + 
    geom_tile(data = subset(to_plo, !is.na(to_plo$nobs)), aes(fill = nobs)) +
    geom_tile(data = subset(to_plo,  is.na(to_plo$nobs)), fill = 'black', alpha = 0) 

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

#' @rdname nobsplot
#' 
#' @export 
#' 
#' @method nobsplot tidal
nobsplot.tidal <- function(dat_in, ...){
  
  nobsplot.default(dat_in, ...)
 
}

#' @rdname nobsplot
#' 
#' @export 
#' 
#' @method nobsplot tidalmean
nobsplot.tidalmean <- function(dat_in, ...){
  
  nobsplot.default(dat_in, ...)
 
}