######
#' Get WRTDS peformance metrics
#'
#' Get WRTDS performance metrics indcluding goodness of fit, root mean square error, and normalized mean square error.
#' 
#' @param tidal_in input tidal object which must already have fitted model data
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @details Goodness of fit is calculated using the \code{\link{goodfit}} function for quantile regression described in Koenker and Mochado 1999.  Root mean square error is based on square root of the mean of the squared residuals.  Normalized mean square error described in Gershenfeld and Weigend 1993 is the sum of the squared errors divided by the sum of the non-conditional errors (i.e., sum of the squared values of the observed minus the mean of the observed).  This measure allows comparability of error values for data with different ranges, although the interpretation for quantile models is not clear.  The value is provided as a means of comparison for WRTDS models created from the same data set but with different window widths during model fitting.
#' 
#' Performance metrics are only valid for chlorophyll observations and model residuals in log-space.
#' 
#' @seealso \code{\link{wrtdsres}} for residuals, \code{\link{goodfit}}
#' 
#' @return A \code{\link[base]{data.frame}} with the metrics for each quantile model
#' 
#' @references 
#' Gershenfeld, N.A., Weigend, A.S. 1993. The future of time series: learning and understanding. In: Weigend, A.S., Gershenfeld, N.A. (eds). Time Series Prediction: Forecasting the Future and Understanding the Past., second ed. Addison-Wesley, Santa Fe, New Mexico. pp. 1-70.
#' 
#' Koenker, R., Machado, J.A.F. 1999. Goodness of fit and related inference processes for quantile regression. Journal of the American Statistical Association. 94(448):1296-1310.
#' 
#' @examples
#' ## load a fitted model object
#' data(tidfit)
#' 
#' ## get performance metrics
#' wrtdsperf(tidfit)
wrtdsperf <- function(tidal_in, ...) UseMethod('wrtdsperf')

#' @rdname wrtdsperf
#' 
#' @export
#'
#' @method wrtdsperf tidal
wrtdsperf.tidal<- function(tidal_in, ...){
  
  # sanity check
  if(!is.null(attr(tidal_in, 'bt_fits'))) stop('Incorrect input for quantile models')
  
  if(!any(grepl('^res|^resnl', names(tidal_in))))
    tidal_in <- wrtdsres(tidal_in, trace = FALSE)
  
  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(tidfit, 'fits'))))
  
  # get residuals, back-transform if needed
  res <- tidal_in[, grepl('^res|chla', names(tidal_in))]
  
  # make long format by type
  res$id <- 1:nrow(res)
  res <- tidyr::gather(res, 'key', 'value', -id, -chla)
  res <- tidyr::extract(res, 'key', c('type', 'tau'), '(res|resnl)([0]\\.[0-9])')
  res <- tidyr::spread(res, 'type', 'value')
  res$mod <- as.numeric(res$tau)
   
  # estimate performance measures
  perf <- dplyr::group_by(res, tau)
  perf <- dplyr::summarize(perf, 
    gfit = goodfit(res, resnl, as.numeric(unique(mod))),
    rmse = sqrt(mean(res^2, na.rm = TRUE)),
    nmse = sum(res^2, na.rm = TRUE)/sum((chla - mean(chla, na.rm = TRUE))^2, na.rm = TRUE)
    )
  perf <- data.frame(perf)

  return(perf) 
  
}
