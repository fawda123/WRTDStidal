#' Plot combined predicted and normalized results from a tidal object
#' 
#' Plot combined predicted and normalized results from a tidal object to evaluate the influence of salinity changes on chlorophyll. The plot is similar to that produced by \code{\link{fitplot}} except predicted chlorophyll values are shown as points and observed values are removed.
#' 
#' @param tidal_in input tidal object
#' @param tau numeric vector of quantiles to plot, defaults to all in object if not supplied
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param logspace logical indicating if plots are in log space
#' @param dt_rng Optional chr string indicating the date range of the plot. Must be two values in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @import dplyr ggplot2 RColorBrewer tidyr
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
#' # plot using defaults
#' prdnrmplot(tidfit)
#' 
#' # get the same plot but use default ggplot settings
#' prdnrmplot(tidfit, pretty = FALSE)
#' 
#' # plot in log space
#' prdnrmplot(tidfit, logspace = TRUE)
#' 
#' # plot specific quantiles
#' prdnrmplot(tidfit, tau = c(0.1, 0.9))
#' 
#' # plot the normalized predictions
#' prdnrmplot(tidfit, predicted = FALSE)
#' 
#' # plot as annual aggregations
#' prdnrmplot(tidfit, annuals = TRUE) 
#' 
#' # format the x-axis is using annual aggregations
#' library(ggplot2)
#' 
#' prdnrmplot(tidfit, annual = TRUE) + 
#'  scale_x_continuous(breaks = seq(2000, 2012, by = 4))
#'
#' # modify the plot as needed using ggplot scales, etc.
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
prdnrmplot <- function(tidal_in, ...) UseMethod('prdnrmplot')

#' @rdname prdnrmplot
#' 
#' @export 
#' 
#' @method prdnrmplot tidal
prdnrmplot.tidal <- function(tidal_in, tau = NULL, annuals = FALSE, logspace = FALSE, dt_rng = NULL, pretty = TRUE, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(tidal_in)
  sel_vec <- grepl('^date$|^chla$|^fit|^norm', names(to_plo))
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
  nrms <- gather(to_plo, 'taus', 'nrms_value', tau_nrms) %>% 
    select(date, taus, nrms_value) %>% 
    mutate(taus = gsub('^norm', '', taus))
  fits <- gather(to_plo, 'taus', 'fits_value', tau_fits) %>% 
    select(date, taus, fits_value) %>% 
    mutate(taus = gsub('^fit', '', taus))
  
  # y-axis label
  ylabel <- expression(
    paste('log-Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
    )

  # back-transform if needed
  if(!logspace){
    
    ylabel <- expression(
      paste('Chloropyhll-',italic(a),' (',italic('\u03bc'),'g ',L^-1,')')
      )
    
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
    geom_point() +  
    geom_line(data = nrms, aes(x = date, y = nrms_value, group = taus, 
        colour = taus))#, ...)
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  # special case for three quantiles
  if(length(quants) == 3){
    cols <- brewer.pal(10, 'Spectral')[c(1, 7, 9)]
  } else {
    cols <- grDevices::colorRampPalette(
      brewer.pal(10, 'Spectral')
      )(length(quants))
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