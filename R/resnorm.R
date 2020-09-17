######
#' Get salinity/flow normalized WRTDS predictions from interpolation grids
#' 
#' Get normalized model predictions from WRTDS to remove the effect of salinity/flow on the response variable.  Predicted values in the interpolation grids are averaged across dates.
#' 
#' @param dat_in input tidal or tidalmean object
#' @param trace logical indicating if progress is shown in the console
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @import dplyr
#' 
#' @details
#' This function is used after \code{wrtds} to normalize predicted values of the response variable from the interpolation grid for each model.  The normalized values are based on the average of all predicted estimates across the range of salinity/flow values that have occurred on the same date throughout each year.  For example, normalized values for July 2000 are the mean predicted response at that date using the observed salinity/flow values that occur in July of all years.  The normalized values allow an interpretation of trends in the response variable that are independent of changes in salinity or freshwater inputs.  
#' 
#' @return Appends columns to the data.frame for normalized values.  For, tidal objects, columns are named starting with the prefix `norm', e.g., `norm0.5' are the normalized values for the fit through the median.  For tidalmean objects, columns are appended for the log-transformed and back-transformed normalized values, named `norm' and `bt_norm'.
#'  
#' @examples
#' \dontrun{
#' ##
#' 
#' # load a tidal object
#' data(tidobj)
#' 
#' # get flow-normalized values for each quantile
#' res <- resnorm(tidobj)
#'
#' # load a tidalmean object
#' data(tidobjmean)
#' 
#' # get flow-normalized values
#' res <- resnorm(tidobjmean)
#' }
resnorm <- function(dat_in, ...) UseMethod('resnorm')

#' @rdname resnorm
#'
#' @export
#'
#' @method resnorm tidal
#' 
resnorm.tidal <- function(dat_in, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  num_obs <- nrow(dat_in)
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')
  
  if(trace){
    cat('\nNormalizing predictions...\n\n')
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }
  
  # quantiles to predict
  tau <- names(fits)
  
  # flo values occuring by month, used for interpolation
  flo_mon <- select(dat_in, date, flo) %>% 
    mutate(
      yr = lubridate::year(date), 
      mo = lubridate::month(date)
    ) %>% 
    select(-date) %>% 
    group_by(yr, mo) %>% 
    summarize(flo = mean(flo, na.rm = TRUE)) %>% 
    tidyr::spread(yr, flo)
  
  # values to interpolate for normalization
  to_norm <- select(dat_in, date) %>% 
    mutate(mo = lubridate::month(date)) %>% 
    left_join(., flo_mon, by = 'mo') %>% 
    tidyr::gather('yr', 'flo', -date, -mo) %>% 
    arrange(date) %>% 
    select(date, flo) %>% 
    na.omit
  
  # normalize predictions for each quantile
  for(i in seq_along(tau)){
  
    if(trace){
      txt <- paste0('tau = ', gsub('fit', '', tau[i]), '\n')
      cat(txt)
    }
    
    # interp grid and flo values to interp
    fit_grd <- fits[[i]]
    dts <- fit_grd$date
    fit_grd <- select(fit_grd, -year, -month, -day, -date)

    # bilinear interpolatoin of fit grid with data to average for norms
    norms <- interp.surface(
      obj = list(
        y = attr(dat_in, 'flo_grd'),
        x = dts,
        z = fit_grd
      ), 
      loc = to_norm
    )
    
    # append to normalization data, then average for unique dates
    # averaging happens only if more than 80% of the predictions for a date are filled
    norms <- data.frame(date = to_norm$date, norms) %>% 
      group_by(date) %>% 
      summarise(norm = ifelse(sum(is.na(norms))/length(norms) > 0.2, NA, mean(norms, na.rm = TRUE)))
   
    # append to dat_in object for the grid
    dat_in <- dplyr::left_join(dat_in, norms, by = 'date')
    colnm <- gsub('^fit', 'norm', tau[i])
    names(dat_in)[grepl('^norm$', names(dat_in))] <- colnm
   
    if(trace) cat('\n')
    
  }
  
  # exit function
  return(dat_in)
    
}

#' @rdname resnorm
#'
#' @export
#'
#' @method resnorm tidalmean
resnorm.tidalmean <- function(dat_in, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  bt_fits <- attr(dat_in, 'bt_fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  num_obs <- nrow(dat_in)
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')

  if(trace){
    cat('\nNormalizing predictions...\n\n')
    counts <- round(seq(1, nrow(dat_in), length = 20))
  }

  # prep interp grids by adding month, year columns
  fit_grd <- fits[[1]]
  dts <- fit_grd$date
  fit_grd <- select(fit_grd, -year, -month, -day, -date)
  btfit_grd <- bt_fits[[1]] %>% 
    select(-year, -date, -month, -day)

  # flo values occuring by month, used for interpolation
  flo_mon <- select(dat_in, date, flo) %>% 
    mutate(
      yr = lubridate::year(date), 
      mo = lubridate::month(date)
    ) %>% 
    select(-date) %>% 
    group_by(yr, mo) %>% 
    summarize(flo = mean(flo, na.rm = TRUE)) %>% 
    tidyr::spread(yr, flo)
  
  # values to interpolate for normalization
  to_norm <- select(dat_in, date) %>% 
    mutate(mo = lubridate::month(date)) %>% 
    left_join(., flo_mon, by = 'mo') %>% 
    tidyr::gather('yr', 'flo', -date, -mo) %>% 
    arrange(date) %>% 
    select(date, flo) %>% 
    na.omit
  
  # bilinear interpolatoin of fit grid with data to average for norms
  norms <- interp.surface(
    obj = list(
      y = attr(dat_in, 'flo_grd'),
      x = dts,
      z = fit_grd
    ), 
    loc = to_norm
  )
  
  # append to normalization data, then average for unique dates
  # averaging happens only if more than 80% of the predictions for a date are filled
  norms <- data.frame(date = to_norm$date, norms) %>% 
    group_by(date) %>% 
    summarise(norm = ifelse(sum(is.na(norms))/length(norms) > 0.2, NA, mean(norms, na.rm = TRUE)))
   
  # bilinear interpolatoin of fit grid with data to average for norms
  btnorms <- interp.surface(
    obj = list(
      y = attr(dat_in, 'flo_grd'),
      x = dts,
      z = btfit_grd
    ), 
    loc = to_norm
  )
    
  # append to normalization data, then average for unique dates
  # averaging happens only if more than 80% of the predictions for a date are filled
  btnorms <- data.frame(date = to_norm$date, btnorms) %>% 
    group_by(date) %>% 
    summarise(bt_norm = ifelse(sum(is.na(btnorms))/length(btnorms) > 0.2, NA, mean(btnorms, na.rm = TRUE)))
   
  # append to dat_in object
  dat_in <- dplyr::left_join(dat_in, norms, by = 'date')
  dat_in <- dplyr::left_join(dat_in, btnorms, by = 'date')

  # exit function
  return(dat_in)
    
}