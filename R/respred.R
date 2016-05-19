######
#' Get WRTDS predictions from interpolation grids
#' 
#' Get model predictions from WRTDS using linear interpolation of values in grids
#' 
#' @param dat_in input tidal or tidalmean object
#' @param dat_pred optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied, see details
#' @param trace logical indicating if progress is shown in the console
#' @param omit logical indicating if observations in \code{dat_pred} that are outside of the range of data used to fit the model are removed, see details
#' @param ... arguments passed to or from other methods
#' 
#' @import dplyr fields
#' 
#' @export
#' 
#' @details
#' This function is used after \code{wrtds} to estimate predicted values of the response variable from the interpolation grids.  The estimated values are based on a bilinear interpolation of the four predicted response values at two salinity/flow and two date values nearest to the observed salinity/flow and date values to predict.  
#' 
#' Data for \code{dat_pred} must be a data frame of two columns for date and flow variables (\code{date} and \code{numeric} objects).  The columns must be named 'date' and 'flo'.  Values that are outside of the range of data used to fit the model are removed with a warning.  It is assumed that the flow variable is not scaled (i.e., raw data) as in a \code{tidal} or \code{tidalmean} object. The dimensions of the output data are modified to match \code{dat_pred} if observations are removed.  The \code{omit} argument should not equal \code{FALSE} and is included only for use with \code{\link{wrtdscv}} to evaluate folds of the original dataset.
#' 
#' @return Appends columns to the input data.frame for the predicted values.  For tidal objects, columns are named starting with the prefix `fit', e.g., `fit0.5' are the predicted values for the fit through the median.  For tidalmean objects, predicted values are appended for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `fits' and `bt_fits'.
#'  
#' @examples
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get fitted values for each quantile
#' res <- respred(tidobj)
#' 
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get predicted values
#' res <- respred(tidobjmean)
#' 
respred <- function(dat_in, ...) UseMethod('respred')

#' @rdname respred
#'
#' @export
#'
#' @method respred tidal
respred.tidal <- function(dat_in, dat_pred = NULL, trace = TRUE, omit = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace){
    cat('\nEstimating predictions...\n\n')
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # quantiles to predict
  tau <- names(fits)

  # data to predict, uses dat_in if dat_pred is NULL
  # also creates empty list for output if not NULL
  if(is.null(dat_pred)){ 
    
    to_pred <- dat_in
    
  } else{
    
    # get predictions from observed data for predonobs attributes
    # only relevant if novel prediction data used
    dat_in <- fillpo(dat_in)
    
    # remove NA if present
    dat_pred <- na.omit(dat_pred)
    
    # stop if names are incorrect for input data
    if(!any(names(dat_pred) %in% c('flo', 'date')))
      stop('Names in data to predict must be flo and date')

    out_pred <- vector('list', length(tau))
    names(out_pred) <- tau
    
    # dat_pred flo must be in same range as flo used to fit mod
    # those outside of range are removed
    dat_pred$flo <- as.numeric(dat_pred$flo)
    if(omit){
      
      floobs_rng <- attr(dat_in, 'floobs_rng')
      torm <- with(dat_pred, flo < floobs_rng[1] | flo > floobs_rng[2])
      if(any(torm))
        warning(paste('Data for prediction outside of range are removed:' , sum(torm), 'obs'))
      
      dat_pred <- dat_pred[!torm, ]
      dat_pred$flo <- with(dat_pred, (flo - floobs_rng[1])/diff(floobs_rng))
      
    }
    
    to_pred <- dat_pred
    
  }
  to_pred <- to_pred[, c('date', 'flo')]

  # get predictions for each quantile
  for(i in seq_along(tau)){
    
    # interp grids, as matrix
    fit_grd <- fits[[i]]
    dts <- fit_grd$date
    fit_grd <- select(fit_grd, -year, -month, -day, -date)

    if(trace){
      txt <- paste0('tau = ', gsub('fit', '', tau[i]), '\n')
      cat(txt)
    }
    
    # bilinear interpolatoin of fit grid with data to predict
    preds <- interp.surface(
      obj = list(
        y = attr(dat_in, 'flo_grd'),
        x = dts,
        z = fit_grd
      ), 
      loc = to_pred
    )
    
    # add results to input object
    if(is.null(dat_pred)){
      
      # append vector to dat_in object
      dat_in$fits <- preds
      names(dat_in)[grepl('^fits$', names(dat_in))] <- tau[i]
      
    } else {
      
      # add vector of novel prediction to list
      out_pred[[i]] <- preds
      
    }
    
    if(trace) cat('\n')

  }

  # exit function, return original object with fits, otherwise expand input object with novel data
  if(is.null(dat_pred)){ 
    
    # output is input with predicted data
    out <- dat_in
    
    # add the predictions predonobs attributes for perf metrics
    attr(out, 'predonobs') <- dat_in[, grepl('^res|^not_cens$|^fit', names(dat_in))]

  } else {
    
    # combine predicted with orig data
    out <- do.call('cbind', out_pred)
    out <- data.frame(dat_pred, out)
    out <- full_join(dat_in, out, by = 'date') %>% 
      arrange(date) %>% 
      mutate(flo.x = flo.y) %>% 
      select(-flo.y) %>% 
      rename(flo = flo.x)
    
    # reformat decimal time columns
    dect <- dec_time(out$date)
    out$day_num <- dect$day_num
    out$month <- dect$month
    out$year <- dect$year
    out$dec_time <- dect$dec_time
    
    # some dirty hack to make new data a tidal object with same atts as before
    class(out) <- c('tidal', 'data.frame')
    att_add <- attributes(dat_in)
    attributes(out) <- c(attributes(out), att_add[!names(att_add) %in% c('names', 'row.names',  'class')])
    
  }
  
  return(out)
    
}

#' @rdname respred
#'
#' @export
#'
#' @method respred tidalmean
respred.tidalmean <- function(dat_in, dat_pred = NULL, trace = TRUE, omit = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace){
    cat('\nEstimating predictions... \n\n')
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # interp grids
  fit_grd <- fits[[1]]
  dts <- fit_grd$date
  fit_grd <- select(fit_grd, -year, -month, -day, -date)
  btfit_grd <- bt_fits[[1]] %>% 
    select(-year, -month, -day, -date)

  # data to predict, uses dat_in if dat_pred is NULL
  if(is.null(dat_pred)){
    
    to_pred <- dat_in
    
  } else{
   
    # get predictions from observed data for predonobs attribute
    # only relevant if novel prediction data used
    dat_in <- fillpo(dat_in)
    
    # remove NA if present
    dat_pred <- na.omit(dat_pred)
    
    # stop if names are incorrect for input data
    if(!any(names(dat_pred) %in% c('flo', 'date')))
      stop('Names in data to predict must be flo and date')

    # dat_pred flo must be in same range as flo used to fit mod
    # those outside of range are removed
    dat_pred$flo <- as.numeric(dat_pred$flo)
    if(omit){
      
      floobs_rng <- attr(dat_in, 'floobs_rng')
      torm <- with(dat_pred, flo < floobs_rng[1] | flo > floobs_rng[2])
      if(any(torm))
        warning(paste('Data for prediction outside of range are removed:' , sum(torm), 'obs'))
      
      dat_pred <- dat_pred[!torm, ]
      dat_pred$flo <- with(dat_pred, (flo - floobs_rng[1])/diff(floobs_rng))
    }
    
    to_pred <- dat_pred

  }
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
  
  if(trace) cat('\n')
  
  # create output depending on obs data or predicted data
  if(is.null(dat_pred)){
    
    # append to dat_in object
    dat_in$fits <- preds
    dat_in$bt_fits <- bt_preds
    out <- dat_in
    
    # add the predictions predonobs attributes for perf metrics
    attr(out, 'predonobs') <- dat_in[, grepl('^res|^fit', names(dat_in))]
    
  } else {
    
    # combine predicted with orig data
    out <- as.data.frame(preds, bt_preds)
    names(out) <- c('fits', 'bt_fits')
    out <- data.frame(dat_pred, out)
    out <- full_join(dat_in, out, by = 'date') %>% 
      arrange(date) %>% 
      mutate(flo.x = flo.y) %>% 
      select(-flo.y) %>% 
      rename(flo = flo.x)
    
    # reformat decimal time columns
    dect <- dec_time(out$date)
    out$day_num <- dect$day_num
    out$month <- dect$month
    out$year <- dect$year
    out$dec_time <- dect$dec_time
    
    # some dirty hack to make new data a tidalmean object with same atts as before
    class(out) <- c('tidalmean', 'data.frame')
    att_add <- attributes(dat_in)
    attributes(out) <- c(attributes(out), att_add[!names(att_add) %in% c('names', 'row.names',  'class')])
    
  }

  # exit function
  return(out)
    
}
