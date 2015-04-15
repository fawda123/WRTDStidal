#' Plot a tidal object
#' 
#' Plot a tidal object to view chlorophyll observations, predictions, and normalized results.
#' 
#' @param tidal_in input tidal object
#' @param tau numeric vector of quantiles to plot, defaults to all in object if not supplied
#' @param predicted logical indicating if standard predicted values are plotted, default \code{TRUE}, otherwise normalized predictions are plotted
#' @param annuals logical indicating if plots are annual aggregations of results
#' @param logspace logical indicating if plots are in log space
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param pt_sz numeric value indicating size of observed chlorophyll points
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @import dplyr ggplot2 RColorBrewer tidyr
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @seealso \code{\link{prdnrmplot}}, \code{\link{sliceplot}}
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
#' # plot as annual aggregations
#' fitplot(tidfit, annuals = TRUE) 
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
fitplot <- function(tidal_in, ...) UseMethod('fitplot')

#' @rdname fitplot
#' 
#' @export 
#' 
#' @method fitplot tidal
fitplot.tidal <- function(tidal_in, tau = NULL, predicted = TRUE, annuals = FALSE, logspace = FALSE, pretty = TRUE, pt_sz = 2, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(tidal_in)
  sel_vec <- grepl('^date$|^chla$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
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
  nrms <- gather(to_plo, 'nrms_variable', 'nrms_value', tau_nrms) %>% 
    select(date, nrms_variable, nrms_value)
  fits <- gather(to_plo, 'fits_variable', 'fits_value', tau_fits) %>% 
    select(date, fits_variable, fits_value)
  
  # y-axis label
  ylabel <- chllab(logspace)
  
  # back-transform if needed
  if(!logspace){

    to_plo$chla <- exp(to_plo$chla)
    nrms$nrms_value <- exp(nrms$nrms_value)
    fits$fits_value <- exp(fits$fits_value)
    
  }
  
  # formatting for quantile legend labels
  quants <- gsub('^fit', '', names(to_plo)[tau_fits])
  quants <- lapply(as.list(quants), 
    function(x) bquote(italic('\u03c4') ~ .(x))
  )
  
  # bare bones plot
  p <- ggplot(to_plo, aes(x = date, y = chla)) + 
    geom_point(aes(size = 'Observed')) + 
    scale_size_manual('', values = pt_sz)
      
  # plot fits or nrms
  if(predicted){
    p <- p + 
      geom_line(data = fits, aes(y = fits_value, group = fits_variable, 
        colour = fits_variable), ...)
    
    leglab <- c('Predicted')
    
  } else {
    p <- p + 
      geom_line(data = nrms, aes(y = nrms_value, group = nrms_variable, 
        colour = nrms_variable), ...)
    
    leglab <- c('Normalized')
  }
  
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
      name = leglab,
      labels=quants,
      values = cols, 
      guide = guide_legend(reverse = TRUE)
    ) +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous(ylabel)
  
  return(p)
  
}