######
#' Get WRTDS residuals
#'
#' Get WRTDS residuals for each quantile model. These are used to estimate goodness of fit of the model predictions.
#' 
#' @param tidal_in input tidal object which must already have fitted model data
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
wrtdsres <- function(tidal_in, ...) UseMethod('wrtdsres')

#' @rdname wrtdsres
#'
#' @import quantreg
#' 
#' @export
#'
#' @method wrtdsres tidal
wrtdsres.tidal<- function(tidal_in, trace = TRUE, ...){
  
  # sanity check
  if(!is.null(attr(tidal_in, 'bt_fits'))) stop('Incorrect input for quantile models')
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(tidfit, 'fits'))))
  
  # null model
  mod_nl <- quantreg::crq(
    Surv(chla, not_cens, type = "left") ~ 1, 
    data = tidal_in, 
    method = "Portnoy"
    )

  # residuals from null model
  parms <- data.frame(coef(mod_nl, tau))
  res_nl <- apply(parms, 1, function(x) tidal_in$chla - x)
  
  # get residuals for conditional quantile models
  res <- apply(
    tidal_in[, paste0('fit', tau)], 
    2, 
    function(x) tidal_in$chla - x
    )
  
  # create output
  nms <- c(paste0('res', tau), paste0('resnl', tau))
  res_out <- data.frame(res, res_nl)
  names(res_out) <- nms
  
  # append to tidal object
  tidal_in[, nms] <- res_out
  
  return(tidal_in) 
  
}
