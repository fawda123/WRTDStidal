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
#' @return A tidal object with columns for the residuals ('res') and non-conditional residuals ('resnl') of each quantile model.
#' 
#' @references Koenker, R., Machado, J.A.F. 1999. Goodness of fit and related inference processes for quantile regression. Journal of the American Statistical Association. 94(448):1296-1310.
#' 
#' @examples
#' ## load a fitted model object
#' data(tidfit)
#' 
#' ## run the function
#' res <- wrtdsres(tidfit)
#' head(res)
wrtdsres <- function(dat_in, ...) UseMethod('wrtdsres')

#' @rdname wrtdsres
#'
#' @import quantreg
#' 
#' @export
#'
#' @method wrtdsres tidal
wrtdsres.tidal <- function(dat_in, trace = TRUE, ...){
  
  # sanity check
  if(!is.null(attr(dat_in, 'bt_fits'))) stop('Incorrect input for quantile models')
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(tidfit, 'fits'))))
  
  # null model
  mod_nl <- quantreg::crq(
    Surv(chla, not_cens, type = "left") ~ 1, 
    data = dat_in, 
    method = "Portnoy"
    )
  
  # residuals from null model
  parms <- data.frame(coef(mod_nl, tau))
  res_nl <- apply(parms, 1, function(x) dat_in$chla - x)
  
  # get residuals for conditional quantile models
  res <- apply(
    dat_in[, paste0('fit', tau)], 
    2, 
    function(x) dat_in$chla - x
    )
  
  # create output
  nms <- c(paste0('res', tau), paste0('resnl', tau))
  res_out <- data.frame(res, res_nl)
  names(res_out) <- nms
  
  # append to tidal object
  dat_in[, nms] <- res_out
  
  return(dat_in) 
  
}
