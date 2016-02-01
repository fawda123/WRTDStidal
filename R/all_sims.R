#' Simulate a chlorophyll time series using all functions
#'
#' Simulate a chlorophyll time series using all simulation functions
#'
#' @param dat_in input \code{\link[base]{data.frame}} that must include discharge and decimal time columns, see example dataset \code{\link{daydat}}
#' @param ... additional arguments passed to \code{\link{lnchla_sim}}
#' 
#' @details This is a convenience function that combines \code{\link{lnQ_sim}}, \code{\link{lnchla_err}}, and \code{\link{lnchla_sim}}.  See the help documentation function for more details on each.
#' 
#' @return Original data frame with additional columns for simulated discharge (\code{lnQ_sim}), the random errors of chlorophyll (\code{errs}), the standard error estimates for each residual (\code{scls}), a flow-independent chlorophyll time series (\code{lnchla_noQ}), and a flow-dependent time series (\code{lnchla_Q}).
#' 
#' @export
#' 
#' @seealso \code{\link{daydat}} for example data, \code{\link{lnQ_sim}} for simulating discharge, \code{\link{lnchla_err}} for estimating the eror structure of chlorophyll, and \code{\link{lnchla_sim}} for simulating chlorophyll
#' 
#' @import dplyr
#' 
#' @examples
#' \dontrun{
#' ## example data
#' data(daydat)
#' 
#' ## simulate
#' tmp <- all_sims(daydat)
#' }
all_sims <- function(dat_in, ...){

  out <- lnQ_sim(dat_in) %>% 
    lnchla_err
  out <- lnchla_sim(out, ...)
 
  return(out)
 
}
