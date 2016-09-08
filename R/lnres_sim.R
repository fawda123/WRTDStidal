#' Simulate a water quality time series
#' 
#' Simulate a water quality time series with an estimated error structure and simulated discharge effect
#'
#' @param dat_in input \code{\link[base]{data.frame}} that must include estimated error and simulated discharge time series, see \code{\link{lnres_err}} and \code{\link{lnQ_sim}} respectively
#' @param lnQ_coef numeric vector of coefficients of the same length of the time series in \code{dat_in} that is multiplied by the discharge vector, see details
#' 
#' @details This function creates a simulated water quality time series and requires error estimates for an observed water quality dataset and a simulated discharge time series.  The water quality time series is created as the additive combination of a seasonal, stationary time component (as in \code{\link{lnQ_sim}} for discharge), a random error component from \code{\link{lnres_err}}, and a simulated discharge time series from \code{\link{lnQ_sim}}.  The discharge time series is considered an explicit component of the water quality time series and is first centered at zero prior to adding.  The optional vector of coefficients passed to \code{lnQ_coef} can mediate the influence of discharge on the water quality time series.  For example, a vector of all zeroes implies no effect, whereas a vector of all ones implies a constant effect (default).
#' 
#' @return The original data frame with additional columns for the seasonal water quality model (\code{lnres_seas}), a flow-independent water quality time series (\code{lnres_noQ}), and a flow-dependent time series (\code{lnres_Q}).
#' 
#' @export
#' 
#' @seealso \code{\link{daydat}} for the format of an input dataset, \code{\link{lnQ_sim}} for simulating discharge, and \code{\link{lnres_err}} for estimating the error distribution of the water quality time series, \code{\link{all_sims}} for completing all steps at once.
#' 
#' @examples 
#' \dontrun{
#' ## example data
#' data(daydat)
#' 
#' ## get simulated discharge
#' sims <- lnQ_sim(daydat)
#' 
#' ## get error structure of wq time series
#' sims <- lnres_err(sims)
#' 
#' ## get simulated wq time series using results from previous
#' lnres_sim(sims)
#'}
lnres_sim <- function(dat_in, lnQ_coef = NULL){

  if(!'errs' %in% names(dat_in)) 
    stop('Need error simulation from water quality residuals')
  
  if(!'lnQ_sim' %in% names(dat_in)) 
    stop('Need simulated flow data')
  
  if(is.null(lnQ_coef)) lnQ_coef <- rep(1, length = nrow(dat_in))
  
  # seasonal wq component, no discharge
  lnres_seas <- lm(lnres ~ dec_time + sin(2 * pi * dec_time) + cos(2 * pi * dec_time), 
    data = dat_in)
  lnres_noQ <- predict(lnres_seas) + with(dat_in, scls * errs)
  
  # add discharge, rescale 
  lnres_Q <- as.numeric(lnres_noQ + with(dat_in, lnQ_coef * scale(lnQ_sim, scale = FALSE)))
  
  dat_in$lnres_seas <- predict(lnres_seas)
  dat_in$lnres_noQ <- lnres_noQ
  dat_in$lnres_Q <- lnres_Q
  
  return(dat_in)
  
}