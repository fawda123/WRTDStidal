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
#' @return Columns are added to the data of the tidal object for the residuals and non-conditional residuals of each observation.  This process is not as simple as subtracting observed minus predicted values for two reasons.  First, the residual at each observation is specific to the weighted regression at the center of the window.  This means that the WRTDS models must be refit to get the residuals.  Second, the goodness of fit measure described in Koenker and Machado (1999) for quantile regression requires a non-conditional quantile for comparison with the conditional quantile.  The non-conditional quantile is not as simple as estimating the relevant quantile for all data in the response since the weights are not considered as during the model fitting process.  Overall, the function is not as computationally intense as \code{\link{wrtds}} since values are only estimated for the predictions as compared to the entire interpolation grid for a quantile. 
#' 
#' @seealso \code{\link{wrtds}}
#' 
#' @return A tidal object with columns for the residuals ('res') and non-conditional residuals ('resnl') of each quantile model.
#' 
#' @references Koenker, R., Machado, J.A.F. 1999. Goodness of fit and related inference processes for quantile regression. Journal of the American Statistical Association. 94(448):1296-1310.
#' 
#' @examples
#' \dontrun{
#' ## load a fitted model object
#' data(tidfit)
#' 
#' ## run the function
#' wrtdsres(tidfit)
#' }
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
  if(!any(grepl('^fit|^norm', names(tidal_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # original window half-widths
  half_wins <- attr(tidal_in, 'half_wins')
  
  # get taus from model
  tau <- as.numeric(gsub('^fit', '', names(attr(tidfit, 'fits'))))
  
  # progress
  if(trace){
    txt <- paste(tau, collapse = ', ')
    txt <- paste0('\nEstimating residuals for tau = ', txt, ', % complete...\n\n')
    cat(txt)
    counts <- round(seq(1, nrow(tidal_in), length = 20))
  }
  
  # output matrix
  res_out <- matrix(nrow = nrow(tidal_in), ncol = length(tau) *2)
  
  # iterate through rows of tidal_in
  for(row in 1:nrow(tidal_in)){
    
    ref_in <- tidal_in[row, ]
    
    # progress
    if(trace){
      perc <- 5 * which(row == counts)
      if(length(perc) != 0) cat(perc, '\t')
    }
  
    ref_wts <- getwts(tidal_in, ref_in, wins = half_wins)
    
    # crq model, estimates all quants
    mod <- quantreg::crq(
      Surv(chla, not_cens, type = "left") ~ 
        dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
      weights = ref_wts,
      data = tidal_in, 
      method = "Portnoy"
      )
    
    # null model
    mod_nl <- quantreg::crq(
      Surv(chla, not_cens, type = "left") ~ 1, 
      weights = ref_wts,
      data = tidal_in, 
      method = "Portnoy"
      )
    
    # test if model worked
    test <- try({coef(mod)})
    if('try-error' %in% class(test)) next
      
    # test if null model worked
    test_nl <- try({coef(mod_nl)})
    if('try-error' %in% class(test_nl)) next
      
    # fitted coefficients for model and null model
    parms <- data.frame(coef(mod, tau))
    parms_nl <- data.frame(coef(mod_nl, tau))
    
    # predicted values by quantile for model and nullmodel
    fits <- sapply(seq_along(tau), function(x){
        
      modfit <- with(ref_in, 
        parms[1, x] + parms[2, x] * dec_time + parms[3, x] * sal + parms[4, x] * sin(2*pi*dec_time) + parms[5, x] * cos(2*pi*dec_time)
      )
      
      modnlfit <- parms_nl[x, ]
      
      return(c(modfit, modnlfit))
      
    })
    
    # get residauls from and append
    resids <- ref_in$chla - fits
    resids <- c(t(resids))
    res_out[row, ] <- resids
  
  }
  
  # name the output and append to tidal object
  nms <- c(paste0('res', tau), paste0('resnl', tau))
  res_out <- data.frame(res_out)
  names(res_out) <- nms
  tidal_in[, nms] <- res_out
  
  if(trace) cat('\n')
  
  return(tidal_in) 
  
}
