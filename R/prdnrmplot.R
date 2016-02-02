#' Plot combined predicted and normalized results from a tidal object
#' 
  #' Plot combined predicted and normalized results from a tidal object to evaluate the influence of salinity or flow changes on the response variable. The plot is similar to that produced by \code{\link{fitplot}} except predicted values are shown as points and observed values are removed.
#' 
#' @param dat_in input tidal or tidalmean object
#' @param tau numeric vector of quantiles to plot, defaults to all in object if not supplied
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param logspace logical indicating if plots are in log space
#' @param dt_rng Optional chr string indicating the date range of the plot. Must be two values in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}}.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param lwd numeric value indicating width of lines
#' @param size numeric value indicating size of points
#' @param alpha numeric value indicating transparency of points or lines
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @seealso \code{\link{fitplot}}, \code{\link{sliceplot}}
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' ## plot using defaults
#' prdnrmplot(tidfit)
#' 
#' ## get the same plot but use default ggplot settings
#' prdnrmplot(tidfit, pretty = FALSE)
#' 
#' ## plot in log space
#' prdnrmplot(tidfit, logspace = TRUE)
#' 
#' ## plot specific quantiles
#' prdnrmplot(tidfit, tau = c(0.1, 0.9))
#' 
#' ## plot the normalized predictions
#' prdnrmplot(tidfit, predicted = FALSE)
#' 
#' ## plot as monthly values
#' prdnrmplot(tidfit, annuals = FALSE) 
#' 
#' ## format the x-axis is using annual aggregations
#' library(ggplot2)
#' 
#' prdnrmplot(tidfit, annual = TRUE) + 
#'  scale_x_continuous(breaks = seq(2000, 2012, by = 4))
#'
#' ## modify the plot as needed using ggplot scales, etc.
#' prdnrmplot(tidfit, pretty = FALSE, linetype = 'dashed') + 
#'  theme_classic() + 
#'  scale_y_continuous(
#'    'Chlorophyll', 
#'    limits = c(0, 50)
#'    ) +
#'  scale_colour_manual( 
#'    '', 
#'    labels = c('lo', 'md', 'hi'), 
#'    values = c('red', 'green', 'blue'), 
#'    guide = guide_legend(reverse = TRUE)
#'    ) 
#'  
#'  ## plot a tidalmean object
#'  data(tidfitmean)
#'  
#'  prdnrmplot(tidfitmean)
#'  
prdnrmplot <- function(dat_in, ...) UseMethod('prdnrmplot')

#' @rdname prdnrmplot
#' 
#' @export 
#' 
#' @method prdnrmplot tidal
prdnrmplot.tidal <- function(dat_in, tau = NULL, annuals = TRUE, logspace = TRUE, dt_rng = NULL, col_vec = NULL, lwd = 1, size = 2, alpha = 1, pretty = TRUE, ...){
 
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
 
  # annual aggregations if TRUE
  if(annuals){
    to_plo <- mutate(to_plo, date = as.numeric(strftime(date, '%Y'))) %>% 
      group_by(date) %>% 
      summarise_each(funs(mean(., na.rm = TRUE)))
  }
    
  # long format for plotting
  # remove 'norm' and 'fit' to combine mapping
  nrms <- tidyr::gather(to_plo, 'taus', 'nrms_value', tau_nrms) %>% 
    select(date, taus, nrms_value) %>% 
    mutate(taus = gsub('^norm', '', taus))
  fits <- tidyr::gather(to_plo, 'taus', 'fits_value', tau_fits) %>% 
    select(date, taus, fits_value) %>% 
    mutate(taus = gsub('^fit', '', taus))
  
  # y-axis label
  ylabel <- attr(dat_in, 'reslab')

  # back-transform if needed
  if(!logspace){
    
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
  }
  
  # formatting for quantile legend labels
  quants <- gsub('^fit', '', names(to_plo)[tau_fits])
  quants <- lapply(as.list(quants), 
    function(x) bquote(italic('\u03c4' ~ .(x)))
  )
  
  # bare bones plot, fits as points, nrms as lines
  p <- ggplot(fits, aes(x = date, y = fits_value, colour = taus)) + 
    geom_point(size = size, alpha = alpha) +  
    geom_line(data = nrms, aes(x = date, y = nrms_value, group = taus, 
        colour = taus), size = lwd, alpha = alpha)
  
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
      name = 'Normalized (lines)\nPredicted (pts)',
      labels = quants,
      values = cols, 
      guide = guide_legend(reverse = TRUE)
    ) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabel)
  
  return(p)
  
}

#' @rdname prdnrmplot
#' 
#' @export 
#' 
#' @method prdnrmplot tidalmean
prdnrmplot.tidalmean <- function(dat_in, annuals = TRUE, logspace = TRUE, dt_rng = NULL, col_vec = NULL, lwd = 1, size = 2, alpha = 1, pretty = TRUE, ...){
 
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
  nrms <- select(to_plo, date, norm, bt_norm)
  fits <- select(to_plo, date, fits, bt_fits)
  
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
  
  # bare bones plot, fits as points, nrms as lines
  p <- ggplot(fits, aes(x = date, y = fits_variable)) + 
    geom_point(size = size, alpha = alpha, aes(colour = 'fits_variable')) +  
    geom_line(data = nrms, aes(x = date, y = nrms_variable, colour = 'nrms_variable'), 
      size = lwd, alpha = alpha)

  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics

  # pick colors
  # special case for three quantiles
  cols <- gradcols(col_vec = col_vec)
  if(is.null(col_vec)) cols <- cols[c(2, 9)]
    
  p <- p + 
    theme_bw() +
    scale_colour_manual(
      labels = c('Predicted', 'Normalized'),
      values = c(cols[1], cols[length(cols)])
    ) +
    guides(color=guide_legend(override.aes=list(shape=c(16,NA),linetype=c(0,1)))) +
    theme(
      axis.title.x = element_blank(),
      legend.title = element_blank()
      ) +
    scale_y_continuous(ylabel)
  
  return(p)
  
}