#' Plot observed response variable and salinity/flow data
#' 
#' Plot observed response variable and salinity/flow time series from a tidal object
#' 
#' @param dat_in input tidal or tidalmean object
#' @param lines logical indicating if a line plot is used, otherwise points
#' @param logspace logical indicating if plots are in log space
#' @param dt_rng Optional chr string indicating the date range of the plot. Must be two values in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param col chr string of plot color to use
#' @param lwd numeric value indicating width of lines
#' @param size numeric value indicating size of points
#' @param alpha numeric value indicating transparency of points or lines
#' @param ... arguments passed to \code{\link[ggplot2]{geom_line}}
#' 
#' @import ggplot2
#' 
#' @seealso \code{\link{fitplot}}
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
#' obsplot(tidfit)
#'  
#' ## changing default
#' obsplot(tidfit, alpha = 0.5, size = 4, col = 'blue', lines = FALSE)
#' 
#' ## plot a tidalmean object
#' data(tidfitmean)
#' 
#' obsplot(tidfitmean)
obsplot <- function(dat_in, ...) UseMethod('obsplot')

#' @rdname obsplot
#' 
#' @export 
#' 
#' @method obsplot default
obsplot.default <- function(dat_in, lines = TRUE, logspace = TRUE, dt_rng = NULL, pretty = TRUE, col = 'black', lwd = 1, size = 2, alpha = 1, ...){
  
  to_plo <- as.data.frame(dat_in)[, c('date', 'res', 'flo')]
  
  # salinity/flow back to observed range
  floobs_rng <- attr(dat_in, 'floobs_rng')
  to_plo$flo <- to_plo$flo * diff(floobs_rng) + floobs_rng[1]

  # facet labels
  labels <- unlist(attributes(dat_in)[c('reslab', 'flolab')])

  # backtransform res
  if(!logspace){
    
    to_plo$res <- exp(to_plo$res)
   
    # strip log, ln  from res label if there
    labels[1] <- gsub('ln-|log-', '', as.character(labels[1]))
    labels[1] <- as.expression(parse(text = labels[1]))
     
  }
  
  # subset data by dt_rng
  if(!is.null(dt_rng)){ 
   
    dt_rng <- as.Date(dt_rng, format = '%Y-%m-%d')
    if(any(is.na(dt_rng)) & length(dt_rng) != 2)
      stop('Argument for dt_rng must be two-element character string of format "YYYY-mm-dd"')
  
    sel_vec <- with(to_plo, date >= dt_rng[1] & date <= dt_rng[2])
    to_plo <- to_plo[sel_vec, ]
    
  }
  
  # long format
  to_plo <- tidyr::gather(to_plo, 'variable', 'value', c(2:3)) %>% 
    na.omit
  
  # change variable labels for plotting facet
  to_plo$variable <- factor(to_plo$variable, 
    levels = c('res', 'flo'), 
    labels = labels
    )
  
  # plot
  base <- ggplot(to_plo, aes(x = date, y = value)) + 
    facet_grid(variable ~ ., scales = 'free_y')
  
  # points or lines
  if(lines) p <- base + geom_line(alpha = alpha, size = lwd) 
  else p <- base + geom_point(alpha = alpha,  size = size)
  
  # return if not pretty
  if(!pretty) return(p)
  
  # recreate for colors
  if(lines) p <- base + geom_line(alpha = alpha, size = lwd, colour = col) 
  else p <- base + geom_point(alpha = alpha,  size = size, colour = col)
  
  # final
  p <- p + 
    facet_grid(variable ~ ., scales = 'free_y', labeller = label_parsed) +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  return(p)
  
}

#' @rdname obsplot
#' 
#' @export 
#' 
#' @method obsplot tidal
obsplot.tidal <- function(dat_in, ...){
  
  obsplot.default(dat_in, ...)
 
}

#' @rdname obsplot
#' 
#' @export 
#' 
#' @method obsplot tidalmean
obsplot.tidalmean <- function(dat_in, ...){
  
  obsplot.default(dat_in, ...)
 
}