#' Simulate a chlorophyll time series
#' 
#' Simulate a chlorophyll time series with an estimated error structure and simulated discharge effect
#'
#' @param dat_in input \code{\link[base]{data.frame}} that must include estimated error and simulated discharge time series, see \code{\link{lnchla_err}} and \code{\link{lnQ_sim}} respectively
#' @param lnQ_coef numeric vector of coefficients of the same length of the time series in \code{dat_in} that is multiplied by the discharge vector, see details
#' 
#' @details This function creates a simulated chlorophyll time series and requires error estimates for an observed chlorophyll dataset and a simulated discharge time series.  The chlorophyll time series is created as the additive combination of a seasonal, stationary time component (as in \code{\link{lnQ_sim}} for discharge), a random error component from \code{\link{lnchla_err}}, and a simulated discharge time series from \code{\link{lnQ_sim}}.  The discharge time series is considered an explicit component of the chlorophyll time series and is first centered at zerio prior to adding.  The optional vector of coefficients passed to \code{lnQ_coef} can mediate the influence of discharge on chlorophyll.  For example, a vector of all zeroes implies no effect, whereas a vector of all ones implies a constant effect (default).
#' 
#' @return The original data frame with additional columns for a flow-independent chlorophyll time series (\code{lnchla_noQ}) and a flow-dependent time series (\code{lnchla_Q}).
#' 
#' @export
#' 
#' @seealso \code{\link{daydat}} for the format of an input dataset, \code{\link{lnQ_sim}} for simulating discharge, and \code{\link{lnchla_err}} for estimating the error distribution of chlorophyll, \code{\link{all_sims}} for completing all steps at once.
#' 
#' @examples 
#' \dontrun{
#' ## example data
#' data(daydat)
#' 
#' ## get simulated discharge
#' sims <- lnQ_sim(daydat)
#' 
#' ## get chlorophyll error structure
#' sims <- lnchla_err(sims)
#' 
#' ## get simulated chl time series using results from previous
#' tmp <- lnchla_sim(sims)
#'}
lnchla_sim <- function(dat_in, lnQ_coef = NULL){

  if(!'errs' %in% names(dat_in)) 
    stop('Need error simulation from chlorophyll residuals')
  
  if(!'lnQ_sim' %in% names(dat_in)) 
    stop('Need simulated flow data')
  
  if(is.null(lnQ_coef)) lnQ_coef <- rep(1, length = nrow(dat_in))
  
  # seasonal chla component, no discharge
  lnchla_noQ <- lm(lnchla ~ dec_time + sin(2 * pi * dec_time) + cos(2 * pi * dec_time), 
    data = dat_in)
  lnchla_noQ <- predict(lnchla_noQ) + with(dat_in, scls * errs)
  
  # add discharge, rescale 
  lnchla_Q <- lnchla_noQ + with(dat_in, lnQ_coef * scale(lnQ_sim, scale = FALSE))
  
  dat_in$lnchla_noQ <- lnchla_noQ
  dat_in$lnchla_Q <- lnchla_Q
  
  return(dat_in)
  
}