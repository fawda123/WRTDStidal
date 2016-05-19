######
#' Get WRTDS residuals
#'
#' Get WRTDS residuals for each quantile model. These are used to estimate goodness of fit of the model predictions.
#' 
#' @param dat_in input tidal object which must already have fitted model data
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @return Columns are added to the data of the tidal object for residuals and non-conditional residuals. Both are required to assess the goodness of fit measure described for quantile regression in Koenker and Machado (1999).
#' 
#' @seealso \code{\link{wrtds}}, \code{\link{wrtdsperf}}, \code{\link{goodfit}}
#' 
#' @return A tidal object with columns added to the \code{predonobs} attribute for the residuals ('rsd') and non-conditional residuals ('rsdnl') of each quantile model or a tidalmean object with columns added to the \code{predonobs} attribute for the residuals ('rsd') and back-transformed residuals ('bt_rsd').
#' 
#' @references Koenker, R., Machado, J.A.F. 1999. Goodness of fit and related inference processes for quantile regression. Journal of the American Statistical Association. 94(448):1296-1310.
#' 
#' @examples
#' ## load a fitted model object
#' data(tidfit)
#' 
#' ## run the function
#' res <- wrtdsrsd(tidfit)
#' head(res)
wrtdsrsd <- function(dat_in, ...) UseMethod('wrtdsrsd')

#' @rdname wrtdsrsd
#'
#' @import quantreg
#' 
#' @export
#'
#' @method wrtdsrsd tidal
wrtdsrsd.tidal <- function(dat_in, trace = TRUE, ...){

  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # use predonobs attr for resids
  predonobs <- attr(dat_in, 'predonobs')  

  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(dat_in, 'fits'))))
  
  # null model
  mod_nl <- quantreg::crq(
    survival::Surv(res, not_cens, type = "left") ~ 1, 
    data = predonobs, 
    method = "Portnoy"
    )
  
  # residuals from null model
  parms <- data.frame(coef(mod_nl, tau))
  rsd_nl <- apply(parms, 1, function(x) predonobs$res - x)
  
  # get residuals for conditional quantile models
  rsd <- apply(
    predonobs[, paste0('fit', tau)], 
    2, 
    function(x) predonobs$res - x
    )
  
  # create output
  nms <- c(paste0('rsd', tau), paste0('rsdnl', tau))
  rsd_out <- data.frame(rsd, rsd_nl)
  names(rsd_out) <- nms
  
  # append to predonobs attribute tidal object
  predonobs[, nms] <- rsd_out
  attr(dat_in, 'predonobs') <- predonobs
  
  return(dat_in) 
  
}

#' @rdname wrtdsrsd
#' 
#' @export
#'
#' @method wrtdsrsd tidalmean
wrtdsrsd.tidalmean <- function(dat_in, trace = TRUE, ...){
  
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  predonobs <- attr(dat_in, 'predonobs')
  
  # get residuals for predicted and backtransformed predicted
  # att to predonobs
  predonobs$rsd <- with(predonobs, res - fits)
  predonobs$bt_rsd <- with(predonobs, exp(res) - bt_fits)
  
  # update attribute
  attr(dat_in, 'predonobs') <- predonobs
  
  return(dat_in) 
  
}

