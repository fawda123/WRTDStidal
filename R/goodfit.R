######
#' Quantile regression goodness of fit
#'
#' Calculate quantile regression goodness of fit using residuals and non-conditional residuals
#' 
#' @param resid numeric vector of residuals from the conditional quantile model
#' @param resid_nl numeric vector of residuals from the non-conditional (null) quantile model
#' @param tau numeric value from zero to one for the estimated quantile
#' 
#' @export
#' 
#' @details The goodness of fit measure for quantile regression is estimated as 1 minus the ratio between the sum of absolute deviations in the fully parameterized models and the sum of absolute deviations in the null (non-conditional) quantile model.  The values are useful for comparisons between quantile models, but they are not comparable to standard coefficients of determination. The latter is based on the variance of squared deviations, whereas goodness of fit values for quantile regression are based on absolute deviations.  Goodness of fit values will always be smaller than R2 values. 
#' 
#' @return A numeric value from 0 to 1 indicating goodness of fit
#' 
#' @seealso \code{\link{wrtdsres}} for residuals
#' 
#' @references Koenker, R., Machado, J.A.F. 1999. Goodness of fit and related inference processes for quantile regression. Journal of the American Statistical Association. 94(448):1296-1310.
#' 
#' @examples
#' 
#' library(quantreg)
#' 
#' ## random variables
#' x <- runif(100, 0, 10)
#' y <- x + rnorm(100)
#' 
#' ## quantile model
#' mod <- rq(y ~ x, tau = 0.5)
#' res <- resid(mod)
#' 
#' ## non-conditional quantile model
#' mod_nl <- rq(y ~ 1, tau = 0.5)
#' res_nl <- resid(mod_nl)
#' 
#' goodfit(res, res_nl, 0.5)
#' 
#' ## r2 of mean model for comparison
#' mod_lm <- lm(y ~ x)
#' 
#' summary(mod_lm)$r.squared
goodfit <- function(resid, resid_nl, tau){
  
  # minimum sum of deviations
  V1 <- resid * (tau - (resid < 0))
  V1 <- sum(V1, na.rm = T) 
  
  # null sum of deviations
  V0 <- resid_nl * (tau - (resid_nl < 0))
  V0 <- sum(V0, na.rm = T) 
  
  # explained deviance
  out <- 1 - V1/V0
  
  # exceptions for output
  if(any(c(Inf, -Inf) %in% out)) out <- NA
  if(V1 > V0) out <- NA
  
  return(out)
  
  }