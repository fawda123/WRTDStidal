#' Plot the weights for an observation
#' 
#' Create several plots showing the weights used to fit a model for a single observation.
#' 
#' @param dat_in input tidal object
#' @param ref chr string indicating the date at the center of the weighting window. Must be in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.  The closest observation is used if the actual is not present in the data.  Defaults to the mean date if not supplied.
#' @param wins list with three elements passed to \code{\link{getwts}} indicating the half-window widths for day, year, and salinity/flow
#' @param min_obs logical to use window widening if less than 100 non-zero weights are found, passed to \code{\link{getwts}}
#' @param slice logical indicating if only weights bounded by the year window (i.e., the limiting window for the combined weights) are shown, passed to \code{\link{getwts}}
#' @param dt_rng Optional chr string indicating the date range for all plots except seasonal (day) weights. Must be two values in the format 'YYYY-mm-dd' which is passed to \code{\link{as.Date}}.
#' @param pt_rng numeric vector of two elements indicating point scaling for all weights in the plot of salinity/flow vs time.
#' @param col_vec chr string of plot colors to use, passed to \code{\link{gradcols}} and \code{\link[ggplot2]{scale_colour_gradientn}} for weight shading.  The last value in the vector is used as the line color if \code{col_lns = NULL}.  Any color palette from RColorBrewer can be used as a named input. Palettes from grDevices must be supplied as the returned string of colors for each palette.
#' @param col_lns chr string of line color in plots
#' @param alpha numeric value from zero to one indicating transparency of points and lines
#' @param as_list logical indicating if plots should be returned in a list
#' @param ... arguments passed to other methods
#' 
#' @details Create diagnostic plots to view the effects of different weighting windows on model predictions.  The plots illustrate the weights that are used when fitting a weighted regression in reference to a single observation.  The process is repeated for all observations when the entire model is fit.  Five plots are produced by the function, each showing the weights in relation to time and the selected observation (i.e., center of the weighting window).  The top plot shows salinity/flow over time with the points colored and sized by the combined weight vector.  The remaining four plots show the weights over time for each separate weighting component (months/days, year, and salinity/flow) and the final combined vector.   
#' 
#' @import ggplot2
#' 
#' @export
#' 
#' @seealso \code{\link{getwts}}
#' 
#' @return A combined \code{\link[ggplot2]{ggplot}} object created using \code{\link[gridExtra]{grid.arrange}}.  A list with elements for each individual plot will be returned if \code{as_list = TRUE}.
#' 
#' @examples
#' 
#' ## load a fitted tidal object
#' data(tidfit)
#' 
#' ## plot using defaults, 
#' wtsplot(tidfit)
#' 
#' ## change the defaults
#' wtsplot(tidfit, ref = '2000-01-01', wins = list(0.5, 15, Inf), 
#'  dt_rng = c('1990-01-01', '2010-01-01'), 
#'  pt_rng = c(3, 8), col_vec = c('lightgreen', 'lightblue', 'purple'),
#'  alpha = 0.7)
wtsplot <- function(dat_in, ...) UseMethod('wtsplot')

#' @rdname wtsplot
#' 
#' @export 
#' 
#' @method wtsplot default
wtsplot.default <- function(dat_in, ref = NULL, wins = list(0.5, 10, NULL), min_obs = TRUE, slice = FALSE, dt_rng = NULL, pt_rng = c(1, 12), col_vec = NULL, col_lns = NULL, alpha = 1, as_list = FALSE, ...){
  
  # format reference position
  if(is.null(ref)){
    
    ref <- as.Date(mean(dat_in$date), format = '%Y-%m-%d', 
      origin = '1970-01-01')
    
  } else {
  
    ref <- as.Date(ref, format = '%Y-%m-%d')
    if(is.na(ref)) 
      stop('Argument for ref date must be character string of format "YYYY-mm-dd"')
    
  }
    
  # get closest observation to referenced
  ref <- which.min(abs(ref - dat_in$dat))[1]
  ref <- dat_in[ref, ]

  # format values for limits on x axis
  if(is.null(dt_rng)){ 
    dt_rng <- range(dat_in$date)
  } else {
   
    dt_rng <- as.Date(dt_rng, format = '%Y-%m-%d')
    if(any(is.na(dt_rng)) & length(dt_rng) != 2)
      stop('Argument for dt_rng must be two-element character string of format "YYYY-mm-dd"')
    
  }
  
  ##
  # get the weights
  ref_wts <- data.frame(
    allwts = getwts(dat_in, ref, wins = wins, min_obs = min_obs, slice = slice),
    getwts(dat_in, ref, wins = wins, all = TRUE, min_obs = min_obs, slice = slice)
  )
  
  # selection vector for year and prep titles
  yr_sub <- dat_in$year == ref$year
  yr_val<- ref$year
  mo_val <- ref$mo
  flo_val <- paste(attr(dat_in, 'flolab'), round(ref$flo, 2))

  # colors
  cols <- gradcols(col_vec = col_vec)
  
  # line color if null
  if(is.null(col_lns))       
    col_lns <- cols[length(cols)]
  
  # month wts
  p1_dat <- data.frame(
    Month = dat_in$date[yr_sub], 
    Wt = ref_wts[yr_sub, 'day_num']
    )
  p1 <- ggplot(p1_dat, aes_string(x = 'Month', y = 'Wt')) + 
    geom_line(colour = col_lns, alpha = alpha) + 
    ggtitle('Month') +
    scale_y_continuous(name = element_blank(), limits = c(0, 1)) +
    scale_x_date(date_labels = '%b', name = element_blank()) +
    theme_bw()
  
  # year wts
  p2_dat <- data.frame(Date = dat_in$date, Wt = ref_wts[, 'dec_time'])
  p2 <- ggplot(p2_dat, aes_string(x = 'Date', y = 'Wt')) + 
    geom_line(colour = col_lns, alpha = alpha) + 
    scale_x_date(name = element_blank(), limits = dt_rng) +
    scale_y_continuous(name = element_blank(), limits = c(0,1)) +
    ggtitle('Year') +
    theme_bw()
  
  # salinity/flow wts
  p3_dat <- data.frame(Date = dat_in$date, Wt = ref_wts[, 'flo'])
  p3 <- ggplot(p3_dat, aes_string(x = 'Date', y = 'Wt')) + 
    geom_line(colour = col_lns, alpha = alpha) + 
    scale_y_continuous(name = element_blank(),limits=c(0,1)) +
    scale_x_date(name = element_blank(), limits = dt_rng) +
    ggtitle(attr(dat_in, 'flolab')) +
    theme_bw()
  
  # all weights
  p4_dat <- data.frame(Date = dat_in$date, Wt = ref_wts[, 'allwts'])
  p4 <- ggplot(p4_dat, aes_string(x = 'Date', y = 'Wt')) + 
    geom_line(colour = col_lns, alpha = alpha) + 
    scale_x_date(name = element_blank(), limits = dt_rng) +
    scale_y_continuous(name = element_blank(),limits=c(0,1)) +
    ggtitle('Combined') + 
    theme_bw()
  
  ##
  #ggplot showing point size and color in relation to total weight
  p_dat <- data.frame(
    dat_in[, c('date', 'res', 'flo')],
    ref_wts
  )
  
  grzero <- sum(p_dat$allwts > 0) 
  p_dat_plo <- ggplot(p_dat, aes_string(x = 'date', y = 'flo', 
      colour = 'allwts', size = 'allwts')) +
    geom_point(alpha = alpha) +
    scale_colour_gradientn(colours = rev(cols)) +
    scale_y_continuous(limits = c(0, max(dat_in$flo)), name = attr(dat_in, 'flolab')) +
    scale_x_date(name = element_blank(), limits = dt_rng) +
    scale_size(range = pt_rng) +
    ggtitle(paste(ref$date, flo_val, paste0(grzero, ' obs with wts > 0'), sep = ', ')) + 
    theme_bw() +
    theme(legend.position = 'none')
  
  # return as list if TRUE
  if(as_list){
    out <- list(p_dat_plo, p1, p2, p3, p4)
    return(out)
  }
  
  # final plot
  gridExtra::grid.arrange(
    p_dat_plo,
    gridExtra::arrangeGrob(p1, p2, p3, p4, nrow = 2, 
      left = grid::textGrob('Weights', rot = 90)),
    bottom = 'Date',
    heights = c(0.7, 1)
    )
    
}

#' @rdname wtsplot
#' 
#' @export 
#' 
#' @method wtsplot tidal
wtsplot.tidal <- function(dat_in, ...){
  
  wtsplot.default(dat_in, ...)
  
}

#' @rdname wtsplot
#' 
#' @export 
#' 
#' @method wtsplot tidalmean
wtsplot.tidalmean <- function(dat_in, ...){
  
  wtsplot.default(dat_in, ...)
  
}