#' Simulate a response variable time series using all functions
#'
#' Simulate a response variable time series using all simulation functions
#'
#' @param dat_in input \code{\link[base]{data.frame}} that must include discharge and decimal time columns, see example dataset \code{\link{daydat}}
#' @param ... additional arguments passed to \code{\link{lnres_sim}}
#' 
#' @details This is a convenience function that combines \code{\link{lnQ_sim}}, \code{\link{lnres_err}}, and \code{\link{lnres_sim}}.  See the help documentation function for more details on each.
#' 
#' @return Original data frame with additional columns for simulated discharge (\code{lnQ_sim}), the random errors of the response variable (\code{errs}), the standard error estimates for each residual (\code{scls}), a flow-independent response variable time series (\code{lnres_noQ}), and a flow-dependent time series (\code{lnres_Q}).
#' 
#' @export
#' 
#' @seealso \code{\link{daydat}} for example data, \code{\link{lnQ_sim}} for simulating discharge, \code{\link{lnres_err}} for estimating the error structure of the response variable, and \code{\link{lnres_sim}} for simulating the response variable
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
    lnres_err
  out <- lnres_sim(out, ...)
 
  return(out)
 
}
