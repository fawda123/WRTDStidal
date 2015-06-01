######
#' Get WRTDS peformance metrics
#'
#' Get WRTDS performance metrics indcluding goodness of fit, root mean square error, and normalized mean square error.
#' 
#' @param dat_in input tidal object which must already have fitted model data
#' @param logspace logical if performance metrics use back-transformed residuals
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
wrtdsperf <- function(dat_in, ...) UseMethod('wrtdsperf')

#' @rdname wrtdsperf
#' 
#' @export
#'
#' @method wrtdsperf tidal
wrtdsperf.tidal <- function(dat_in, logspace = TRUE, ...){
  
  if(!any(grepl('^res|^resnl', names(dat_in))))
    dat_in <- wrtdsres(dat_in, trace = FALSE)
  
  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(tidfit, 'fits'))))
  
  # get residuals, back-transform if needed
  res <- dat_in[, grepl('^res|chla', names(dat_in))]
  
  # make long format by type
  res$id <- 1:nrow(res)
  res <- tidyr::gather(res, 'key', 'value', -id, -chla)
  res <- tidyr::extract(res, 'key', c('type', 'tau'), '(res|resnl)([0]\\.[0-9])')
  res <- tidyr::spread(res, 'type', 'value')
  res$mod <- as.numeric(res$tau)
   
  # get performance measures
  perf <- dplyr::group_by(res, tau)
  
  # rmse, nmse on back-transformed if TRUE
  if(!logspace){
    
    perf <- dplyr::summarize(perf, 
      gfit = goodfit(res, resnl, as.numeric(unique(mod))),
      rmse = sqrt(mean(exp(res)^2, na.rm = TRUE)),
      nmse = sum(exp(res)^2, na.rm = TRUE)/sum((exp(chla) - mean(exp(chla), na.rm = TRUE))^2, na.rm = TRUE)
      )
  
  } else {
    
    perf <- dplyr::summarize(perf, 
      gfit = goodfit(res, resnl, as.numeric(unique(mod))),
      rmse = sqrt(mean(res^2, na.rm = TRUE)),
      nmse = sum(res^2, na.rm = TRUE)/sum((chla - mean(chla, na.rm = TRUE))^2, na.rm = TRUE)
      )
    
  }
  
  perf <- data.frame(perf)
  
  return(perf) 
  
}

#' @rdname wrtdsperf
#' 
#' @export
#'
#' @method wrtdsperf tidalmean
wrtdsperf.tidalmean <- function(dat_in, logspace = TRUE, ...){
  
  if(!any(grepl('^res|^bt_res', names(dat_in))))
    dat_in <- wrtdsres(dat_in, trace = FALSE)
  
  # get residuals, back-transform if needed
  res <- dat_in[, grepl('^res|^bt_res|chla', names(dat_in))]
  
  # get performanc measures
  # use back-transformed, otherwise use logspace
  if(!logspace){
  
    perf <- with(res, c(
      rmse = sqrt(mean(bt_res^2, na.rm = TRUE)),
      nmse = sum(bt_res^2, na.rm = TRUE)/sum((exp(chla) - mean(exp(chla), na.rm = TRUE))^2, na.rm = TRUE)
      ))

  } else {
   
    perf <- with(res, c(
      rmse = sqrt(mean(res^2, na.rm = TRUE)),
      nmse = sum(res^2, na.rm = TRUE)/sum((chla - mean(chla, na.rm = TRUE))^2, na.rm = TRUE)
      ))
     
  }

  perf <- t(data.frame(perf))
  row.names(perf) <- 1:nrow(perf)
  
  return(perf) 
  
}
