#' Akaike's Information Criterion for weighted quantile regression  
#' 
#' Get AIC values for a single weighted quantile regression as used in WRTDS models
#'
#' @param mod_in input crq model
#' @param dat_in input data used to create the model
#' @param tau numeric indicating quantile to evaluate
#' 
#' @export
#' 
#' @details The AIC value is based on the log-likelihood estimate of the model that accounts for the specific quantile, the minimum of the objective function (rho), and the number of model parameters.  The residuals are specific to the WRTDS model such that this function cannot be applied to arbitrary crq models.     
#'
#' @return AIC estimate
#'
#' @examples
#' # get wts for a model centered on the first observation
#' ref_in <- tidobj[1, ]
#' ref_wts <- getwts(tidobj, ref_in)
#' 
#' # get the model
#' mod <- quantreg::crq(
#'    survival::Surv(chla, not_cens, type = "left") ~ 
#'      dec_time + sal + sin(2*pi*dec_time) + cos(2*pi*dec_time), 
#'    weights = ref_wts,
#'    data = tidobj, 
#'    method = "Portnoy"
#'    )
#' 
#' aiccrq(mod, tidobj)
aiccrq <- function(mod_in, dat_in, tau = 0.5){
  
  # get residuals for tau
  parms <- coef(mod_in, tau)
      
  # predicted values by quantile model coefficients
  fits <- with(dat_in, 
    parms[1] + parms[2] * dec_time + parms[3] * sal + parms[4] * sin(2*pi*dec_time) + parms[5] * cos(2*pi*dec_time)
  )
  
  # residuals
  resid <- dat_in$chla - fits
 
  # minimum sum of deviations
  V1 <- resid * (tau - (resid < 0))
  V1 <- sum(V1, na.rm = T) 
  
  n <- length(resid) # residuals
  p <- length(parms) # number of parameters
  
  val <- n * (log(tau * (1 - tau)) - 1 - log(V1/n))
  attr(val, "n") <- n
  attr(val, "df") <- p
  class(val) <- "logLik"

  aic_out <- AIC(val) # -2 * logLik + 2 * p

  return(aic_out)
  
}