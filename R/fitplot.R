#' Plot the fitted results for a tidal object
#' 
#' Plot a tidal object to view response variable observations, predictions, and normalized results.
#' 
#' @param dat_in input tidal or tidalmean object
#' @param tau numeric vector of quantiles to plot, defaults to all in object if not supplied
#' @param predicted logical indicating if standard predicted values are plotted, default \code{TRUE}, otherwise normalized predictions are plotted
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param dt_rng Optional chr string indicating the date range of the plot. Must be two values in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}}.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param logspace logical indicating if plots are in log space
#' @param grids logical indicating if grid lines are present
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param lwd numeric value indicating width of lines
#' @param size numeric value indicating size of points
#' @param alpha numeric value indicating transparency of points or lines
#' @param ... arguments passed to other methods
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @seealso \code{\link{fitmoplot}}, \code{\link{prdnrmplot}}, \code{\link{sliceplot}}
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' # plot using defaults
#' fitplot(tidfit)
#' 
#' # get the same plot but use default ggplot settings
#' fitplot(tidfit, pretty = FALSE)
#' 
#' # plot in log space
#' fitplot(tidfit, logspace = TRUE)
#' 
#' # plot specific quantiles
#' fitplot(tidfit, tau = c(0.1, 0.9))
#' 
#' # plot the normalized predictions
#' fitplot(tidfit, predicted = FALSE)
#' 
#' # plot as monthly values
#' fitplot(tidfit, annuals = FALSE) 
#' 
#' # format the x-axis is using annual aggregations
#' library(ggplot2)
#' 
#' fitplot(tidfit, annual = TRUE) + 
#'  scale_x_continuous(breaks = seq(2000, 2012, by = 4))
#'
#' # modify the plot as needed using ggplot scales, etc.
#' 
#' fitplot(tidfit, pretty = FALSE, linetype = 'dashed') + 
#'  theme_classic() + 
#'  scale_y_continuous(
#'    'Chlorophyll', 
#'    limits = c(0, 50)
#'    ) +
#'  scale_colour_manual( 
#'    'Predictions', 
#'    labels = c('lo', 'md', 'hi'), 
#'    values = c('red', 'green', 'blue'), 
#'    guide = guide_legend(reverse = TRUE)
#'    ) 
#'  
#' # plot a tidalmean object
#' data(tidfitmean)
#' 
#' fitplot(tidfitmean)  
fitplot <- function(dat_in, ...) UseMethod('fitplot')

#' @rdname fitplot
#' 
#' @export 
#' 
#' @method fitplot tidal
fitplot.tidal <- function(dat_in, tau = NULL, predicted = TRUE, annuals = TRUE, logspace = TRUE, dt_rng = NULL, col_vec = NULL, grids = TRUE, pretty = TRUE, lwd = 1, size = 2, alpha = 1, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^date$|^res$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # subset data by dt_rng
  if(!is.null(dt_rng)){ 
   
    dt_rng <- as.Date(dt_rng, format = '%Y-%m-%d')
    if(any(is.na(dt_rng)) & length(dt_rng) != 2)
      stop('Argument for dt_rng must be two-element character string of format "YYYY-mm-dd"')
  
    sel_vec <- with(to_plo, date >= dt_rng[1] & date <= dt_rng[2])
    to_plo <- to_plo[sel_vec, ]
    
  }
  
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
 
  # annual aggregations if TRUE, otherwise monthly agg
  if(annuals){
    to_plo <- mutate(to_plo, date = as.numeric(strftime(date, '%Y'))) %>% 
      group_by(date) %>% 
      summarise_each(funs(mean(., na.rm = TRUE)))
  } 
  
  # long format for plotting
  nrms <- tidyr::gather(to_plo, 'nrms_variable', 'nrms_value', tau_nrms ) %>% 
    select(date, nrms_variable, nrms_value) %>% 
    na.omit
  fits <- tidyr::gather(to_plo, 'fits_variable', 'fits_value', tau_fits) %>% 
    select(date, fits_variable, fits_value) %>% 
    na.omit
  
  # y-axis label
  ylabel <- attr(dat_in, 'reslab')
  
  # back-transform if needed
  if(!logspace){

    to_plo$res <- exp(to_plo$res)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
  }
  
  # formatting for quantile legend labels
  quants <- gsub('^fit', '', names(to_plo)[tau_fits])
  quants <- lapply(as.list(quants), 
    function(x) bquote(italic('\u03c4') ~ .(x))
  )
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = date, y = res)) + 
    geom_point(aes(size = 'Observed'), alpha = alpha, na.rm = TRUE) + 
    scale_size_manual('', values = size)
      
  # plot fits or nrms
  if(predicted){
    p <- p + 
      geom_line(data = fits, aes(y = fits_value, group = fits_variable, 
        colour = fits_variable), size = lwd, alpha = alpha, na.rm = TRUE)
    
    leglab <- c('Predicted')
    
  } else {
    p <- p + 
      geom_line(data = nrms, aes(y = nrms_value, group = nrms_variable, 
        colour = nrms_variable), size = lwd, alpha = alpha, na.rm = TRUE)
    
    leglab <- c('Normalized')
  }
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  # special case for three quantiles
  colpal <- gradcols(col_vec = col_vec)
  cols <- colpal[round(seq(1, length(colpal), length = length(quants)))]
  if(is.null(col_vec)){
    if(length(quants) == 3) cols <- colpal[c(1, 9, 10)]
    if(length(quants) == 2) cols <- colpal[c(1, 9)]
    if(length(quants) == 1) cols <- colpal[c(1)]
  } 
  
  p <- p + 
    theme_bw() +
    scale_colour_manual(
      name = leglab,
      labels=quants,
      values = cols, 
      guide = guide_legend(reverse = TRUE)
    ) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabel)
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}

#' @rdname fitplot
#' 
#' @export 
#' 
#' @method fitplot tidalmean
fitplot.tidalmean <- function(dat_in, predicted = TRUE, annuals = TRUE, logspace = TRUE, dt_rng = NULL, col_vec = NULL, grids = TRUE, pretty = TRUE, lwd = 1, size = 2, alpha = 1, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^date$|^res$|fit|norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # subset data by dt_rng
  if(!is.null(dt_rng)){ 
   
    dt_rng <- as.Date(dt_rng, format = '%Y-%m-%d')
    if(any(is.na(dt_rng)) & length(dt_rng) != 2)
      stop('Argument for dt_rng must be two-element character string of format "YYYY-mm-dd"')
  
    sel_vec <- with(to_plo, date >= dt_rng[1] & date <= dt_rng[2])
    to_plo <- to_plo[sel_vec, ]
    
  }
  
  # annual aggregations if TRUE
  if(annuals){
    to_plo <- mutate(to_plo, date = as.numeric(strftime(date, '%Y'))) %>% 
      group_by(date) %>% 
      summarise_each(funs(mean(., na.rm = TRUE)))
  }
    
  # separate nrms and fits objects for plotting
  nrms <- select(to_plo, date, norm, bt_norm) %>% 
    na.omit
  fits <- select(to_plo, date, fits, bt_fits) %>% 
    na.omit
  
  # y-axis label
  ylabel <- attr(dat_in, 'reslab')
  
  # use back-transformed if TRUE
  if(!logspace){

    to_plo$res <- exp(to_plo$res)
    nrms <- mutate(nrms, nrms_variable = bt_norm)
    nrms <- select(nrms, -norm, -bt_norm)
    fits <- mutate(fits, fits_variable = bt_fits)
    fits <- select(fits, -fits, -bt_fits)
    
  } else {

    nrms <- mutate(nrms, nrms_variable = norm)    
    nrms <- select(nrms, -bt_norm, -norm)
    fits <- mutate(fits, fits_variable = fits)
    fits <- select(fits, -bt_fits, -fits)
    
  }
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = date, y = res)) + 
    geom_point(aes(size = 'Observed'), alpha = alpha, na.rm = TRUE) + 
    scale_size_manual('', values = size)
      
  # plot fits or nrms
  if(predicted){
    p <- p + 
      geom_line(data = fits, aes(y = fits_variable, colour = 'fits_variable'), size = lwd, alpha = alpha, na.rm = TRUE)
    
    leglab <- c('Predicted')
    
  } else {
    p <- p + 
      geom_line(data = nrms, aes(y = nrms_variable, colour = 'nrms_variable'), size = lwd, alpha = alpha, na.rm = TRUE)
    
    leglab <- c('Normalized')
  }

  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  cols <- gradcols(col_vec = col_vec)
  
  p <- p + 
    theme_bw() +
    scale_colour_manual(
      labels = leglab,
      values = cols
    ) +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank()
      ) +
    scale_y_continuous(ylabel)
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}