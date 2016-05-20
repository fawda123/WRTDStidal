#' Fill the \code{predonobs} attribute
#'
#' Fill the \code{predonobs} attribute
#'
#' @param dat_in input tidal or tidalmean object
#' @param ... arguments passed to or from other methods
#' 
#' @details Used in \code{\link{respred}} to fill the \code{predonobs} attribute.  This attribute is used to estimate performance metrics with \code{\link{wrtdsperf}}.
#' 
#' @return The input tidal or tidalmean object with the filled \code{predonobs} attribute as predictions for the observed data as a data frame.
#' 
#' @export
fillpo <- function(dat_in, ...) UseMethod('fillpo')

#' @rdname fillpo
#' 
#' @export
#'
#' @method fillpo tidal
fillpo.tidal <- function(dat_in, ...){
  
  fits <- attr(dat_in, 'fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  predonobs <- attr(dat_in, 'predonobs')
  
  # quantiles to predict
  tau <- names(fits)

  # data to predict, obseved
  to_pred <- dat_in
  to_pred <- to_pred[, c('date', 'flo')]

  # get predictions for each quantile
  for(i in seq_along(tau)){

    # interp grids
    fit_grd <- fits[[i]]
    dts <- fit_grd$date
    fit_grd <- select(fit_grd, -year, -month, -day, -date)

    # bilinear interpolatoin of fit grid with data to predict
    preds <- interp.surface(
      obj = list(
        y = attr(dat_in, 'flo_grd'),
        x = dts,
        z = fit_grd
      ), 
      loc = to_pred
    )

    # append vector to dat_in object
    predonobs <- cbind(predonobs, preds)
  
  }

  # add the preds on observed to attributes of object
  predonobs <- data.frame(res = dat_in$res, not_cens = dat_in$not_cens, predonobs)
  names(predonobs) <- c('res', 'not_cens', tau)
  attr(dat_in, 'predonobs') <- predonobs
  out <- dat_in
    
  return(out)
    
}

#' @rdname fillpo
#' 
#' @export
#'
#' @method fillpo tidalmean
fillpo.tidalmean <- function(dat_in, ...){

  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  
  # interp grids
  fit_grd <- fits[[1]]
  dts <- fit_grd$date
  fit_grd <- select(fit_grd, -year, -month, -day, -date)
  btfit_grd <- bt_fits[[1]] %>% 
    select(-year, -month, -day, -date)
  
  # observed data to predict
  to_pred <- dat_in
  to_pred <- to_pred[, c('date', 'flo')]
  
  # bilinear interpolatoin of fit grid with data to predict
  preds <- interp.surface(
    obj = list(
      y = attr(dat_in, 'flo_grd'),
      x = dts,
      z = fit_grd
    ), 
    loc = to_pred
  )
  
  # bilinear interpolatoin of bt fit grid with data to predict
  bt_preds <- interp.surface(
    obj = list(
      y = attr(dat_in, 'flo_grd'),
      x = dts,
      z = btfit_grd
    ), 
    loc = to_pred
  )     
  
  # add the predictions predonobs attributes for perf metrics
  predonobs <- data.frame(
    res = dat_in$res,
    fits = preds,
    bt_fits = bt_preds
  )
  attr(dat_in, 'predonobs') <- predonobs
  out <- dat_in
    
  return(out)

}