#' Plot seasonal model response by years
#' 
#' Plot seasonal model response by years on a common axis
#' 
#' @param dat_in input tidal or tidalmean object
#' @param years numeric vector of years to plot
#' @param tau numeric vector of quantiles to plot, defaults to all in object if not supplied
#' @param predicted logical indicating if standard predicted values are plotted, default \code{TRUE}, otherwise normalized predictions are plotted
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}}.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param logspace logical indicating if plots are in log space
#' @param grids logical indicating if grid lines are present
#' @param pretty logical indicating if my subjective idea of plot aesthetics is applied, otherwise the \code{\link[ggplot2]{ggplot}} default themes are used
#' @param lwd numeric value indicating width of lines
#' @param alpha numeric value indicating transparency of points or lines
#' @param ... arguments passed to other methods
#' 
#' @details The plot is similar to that produced by \code{\link{seasplot}} except the model estimates are plotted for each year as connected lines, as compared to loess lines fit to the model results.  \code{\link{seasyrplot}} is also similar to \code{\link{sliceplot}} except the x-axis and legend grouping variable are flipped. This is useful for evaluating between-year differences in seasonal trends.
#' 
#' Multiple predictions per month are averaged for a smoother plot. 
#' 
#' Note that the year variable used for color mapping is treated as a continuous variable although it is an integer by definition.
#' 
#' @import dplyr ggplot2 RColorBrewer
#' 
#' @export
#' 
#' @return A \code{\link[ggplot2]{ggplot}} object that can be further modified
#' 
#' @seealso \code{\link{seasplot}}, \code{\link{sliceplot}}
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' # plot using defaults
#' seasyrplot(tidfit)
#' 
#' # get the same plot but use default ggplot settings
#' seasyrplot(tidfit, pretty = FALSE)
#' 
#' # plot specific quantiles
#' seasyrplot(tidfit, tau = c(0.9))
#' 
#' # plot the normalized predictions
#' seasyrplot(tidfit, predicted = FALSE)
#' 
#' # modify the plot as needed using ggplot scales, etc.
#' 
#' library(ggplot2)
#' 
#' seasyrplot(tidfit, pretty = FALSE, linetype = 'dashed') + 
#'  theme_classic() + 
#'  scale_y_continuous(
#'    'Chlorophyll', 
#'    limits = c(0, 5)
#'    )
#'    
#' # plot a tidalmean object
#' data(tidfitmean)
#' 
#' seasyrplot(tidfitmean)    
seasyrplot <- function(dat_in, ...) UseMethod('seasyrplot')

#' @rdname seasyrplot
#' 
#' @export 
#' 
#' @method seasyrplot tidal
seasyrplot.tidal <- function(dat_in, years = NULL, tau = NULL, predicted = TRUE, logspace = TRUE, col_vec = NULL, grids = TRUE, pretty = TRUE, lwd = 0.5, alpha = 1, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^year$|^month$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # subset data by years
  if(!is.null(years)){ 

    to_plo <- filter(to_plo, year %in% years)
    
  }
  
  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_fits <- grep('^fit', names(dat_in))
    tau_fits <- floor(median(tau_fits))
    tau_fits <- names(dat_in)[tau_fits]
    tau_nrms <- gsub('^fit', 'norm', tau_fits)
     
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(dat_in))) == 0)
      stop('Specified tau not in object')
    
    tau_fits <- paste0('fit', tau)
    tau_nrms <- paste0('norm', tau)
    
  }

  # create date vector with common year
  names(to_plo)[names(to_plo) %in% tau_nrms] <- 'nrm'
  names(to_plo)[names(to_plo) %in% tau_fits] <- 'fit'
  to_plo <- select(to_plo, year, month, nrm, fit) %>% 
    group_by(year, month) %>% 
    summarize(
      nrm = mean(nrm, na.rm = TRUE), 
      fit = mean(fit, na.rm = TRUE)
    ) %>% 
    ungroup %>% 
    mutate(
      year_dum = '2000',   
      day = '01'
    ) %>% 
    tidyr::unite('date', year_dum, month, day, sep = '-') %>% 
    mutate(date = as.Date(date, '%Y-%m-%d')) %>% 
    na.omit
  nrms <- select(to_plo, -fit)
  fits <- select(to_plo, -nrm)

  # y-axis label
  ylabel <- attr(dat_in, 'reslab')
  
  # back-transform if needed
  if(!logspace){
    
    nrms$nrm <- exp(nrms$nrm)
    fits$fit <- exp(fits$fit)
    
  }

  # plot fits or nrms
  if(predicted){
    
    p <- ggplot(fits, aes(x = date, y = fit, group = year, colour = year)) + 
      geom_line(alpha = alpha, lwd = lwd, na.rm = TRUE)
    
  } else {
    
    p <- ggplot(nrms, aes(x = date, y = nrm, group = year, colour = year)) + 
      geom_line(alpha = alpha, lwd = lwd, na.rm = TRUE)
    
  }
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  cols <- gradcols(col_vec = col_vec)
  
  # modify aesthetics
  p <- p + 
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) +
    theme_bw() +
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
    ) +
    scale_y_continuous(ylabel) +
    scale_x_date(xlab, date_labels = "%m/%d")

  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}

#' @rdname seasyrplot
#' 
#' @export 
#' 
#' @method seasyrplot tidalmean
seasyrplot.tidalmean <- function(dat_in, years = NULL, tau = NULL, predicted = TRUE, logspace = TRUE, col_vec = NULL, grids = TRUE, pretty = TRUE, lwd = 0.5, alpha = 1, ...){
 
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^year$|^month$|^fits$|^bt_fits$|^norm$|^bt_norm$', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # subset data by years
  if(!is.null(years)){ 

    to_plo <- filter(to_plo, year %in% years)
    
  }

  # create date vector with common year
  to_plo <- group_by(to_plo, year, month) %>% 
    summarize(
      fits = mean(fits, na.rm = TRUE),
      bt_fits = mean(bt_fits, na.rm = TRUE), 
      norm = mean(norm, na.rm = TRUE),
      bt_norm = mean(bt_norm, na.rm = TRUE)
    ) %>% 
    ungroup %>% 
    mutate(
      year_dum = '2000',   
      day = '01'
    ) %>% 
    tidyr::unite('date', year_dum, month, day, sep = '-') %>% 
    mutate(date = as.Date(date, '%Y-%m-%d')) %>% 
    na.omit
  nrms <- select(to_plo, -fits, -bt_fits)
  fits <- select(to_plo, -norm, -bt_norm)

  # y-axis label
  ylabel <- attr(dat_in, 'reslab')
  
  # back-transform if needed
  if(!logspace){
    
    nrms$nrm <- nrms$bt_norm
    fits$fit <- fits$bt_fits
    
  } else {
    
    nrms$nrm <- nrms$norm
    fits$fit <- fits$fits
    
  }

  # plot fits or nrms
  if(predicted){
    
    p <- ggplot(fits, aes(x = date, y = fit, group = year, colour = year)) + 
      geom_line(alpha = alpha, lwd = lwd, na.rm = TRUE)
    
  } else {
    
    p <- ggplot(nrms, aes(x = date, y = nrm, group = year, colour = year)) + 
      geom_line(alpha = alpha, lwd = lwd, na.rm = TRUE)
    
  }
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  cols <- gradcols(col_vec = col_vec)
  
  # modify aesthetics
  p <- p + 
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) +
    theme_bw() +
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
    ) +
    scale_y_continuous(ylabel) +
    scale_x_date(xlab, date_labels = "%m/%d")
  
  # remove grid lines
  if(!grids) 
    p <- p + 
      theme(      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  
  return(p)
  
}