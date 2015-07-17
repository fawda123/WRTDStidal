#' Simulate a discharge time series
#' 
#' Simulate a discharge time series by modelling the statistical properties of an existing daily time series
#'
#' @param dat_in input \code{\link[base]{data.frame}} that must include discharge and decimal time columns, see example dataset \code{\link{daydat}}
#' 
#' @details Daily flow data are simulated as the additive combination of a stationary seasonal component and serially-correlated errors estimated from the observed data.  The stationary seasonal component is based on a seasonal regression of discharge over time.  The residuals from this regression are used to estimate the error distribution using an ARIMA model.  Parameters of the ARIMA model are chosen using stepwise estimation for nonseasonal univariate time series with the \code{\link[forecast]{auto.arima}} function.  Random errors from a standard normal distribution for the length of the original time series are generated using the model estimates with the \code{\link[stats]{arima.sim}} function.  Finally, the errors are multiplied by the standard deviation of the original residuals and added to the seasonal component to create a simulated, daily log-flow time series.  
#' 
#' @return The original data frame with an additional column of simulated data named \code{lnQ_sim}
#' 
#' @export
#' 
#' @import forecast
#' 
#' @seealso \code{\link{daydat}}
#' 
#' @examples 
#' 
#' ## example data
#' data(daydat)
#' 
#' ## simulate
#' lnQ_sim(daydat)
#'
lnQ_sim <- function(dat_in){

  lnQ <- dat_in$lnQ
  dec_time <- dat_in$dec_time
  
  # stationary seasonal model
  seas_mod <- lm(lnQ ~ sin(2 * pi * dec_time) + cos(2 * pi * dec_time))
  seas_fit <- fitted(seas_mod)
  seas_res <- resid(seas_mod)

  # get arma coefficients of resids
  mod <- auto.arima(seas_res, d = 0, seasonal = FALSE)
  ars <- coef(mod)[grep('^ar', names(coef(mod)))]
  mas <- coef(mod)[grep('^ma', names(coef(mod)))]

  # simulate rnorm errors using arma(p,q) process 
  errs <- arima.sim(list(ar = ars, ma = mas, order = c(length(ars), 0, length(mas))), n = nrow(dat_in), 
    rand.gen = function(x) rnorm(x, 0, 1))

  # simulated data, linear trans to range of discharge
  sim_out <- as.numeric({seas_fit +  sd(seas_res) * errs})
  rng <- range(lnQ, na.rm = TRUE)
  sim_out <- rescale(sim_out, to = rng)
  
  dat_in$lnQ_sim <- sim_out
  return(dat_in)
  
}
