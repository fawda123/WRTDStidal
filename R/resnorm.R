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
resnorm.tidal <- function(dat_in, trace = TRUE, ...){
  
  fits <- attr(dat_in, 'fits')
  flo_grd <- attr(dat_in, 'flo_grd')
  num_obs <- nrow(dat_in)
  
  # sanity checks
  if(is.null(fits)) stop('No fits attribute, run wrtds function')

  if(trace) cat('\nNormalizing predictions\n\n')
  
  # quantiles to predict
  tau <- names(fits)
  
  # normalize predictions for each quantile
  for(i in seq_along(tau)){
  
    # interp grid and flo values to interp
    fit_grd <- fits[[i]]

    norms <- rep(NA_real_, num_obs)
    for(row in 1:num_obs){

      # get flo values across all dates for the row
      flo_vals <- flofind(dat_in, row)
      
      # use resinterp for all flo_vals 
      respreds <- sapply(flo_vals, 
        function(x) resinterp(dat_in[row, 'date'], x, fit_grd, flo_grd)
        ) 
      
      # average for normalization and append to norms
      norms[row] <- mean(respreds)
      
    }    

    # append to dat_in object for the grid
    dat_in$norm <- norms
    colnm <- gsub('^fit', 'norm', tau[i])
    names(dat_in)[grepl('^norm$', names(dat_in))] <- colnm
    
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

  if(trace) cat('\nNormalizing predictions\n\n')

  # prep interp grids by adding month, year columns
  fit_grd <- fits[[1]]
  btfit_grd <- bt_fits[[1]]

  norms <- rep(NA_real_, num_obs)
  btnorms <- norms
  for(row in 1:num_obs){

    # get flo values across all dates for the row
    flo_vals <- flofind(dat_in, row)
    
    # use resinterp for all flo_vals 
    respreds <- sapply(flo_vals, 
      function(x){
      
        nrms <- resinterp(dat_in[row, 'date'], x, fit_grd, flo_grd)
        btnrms <- resinterp(dat_in[row, 'date'], x, btfit_grd, flo_grd)
        c(nrms, btnrms)
        
      }
    ) 
    
    # average for normalization and append to norms, btnorms
    norms[row] <- mean(respreds[1, ])
    btnorms[row] <- mean(respreds[2, ])
      
    # append to dat_in object for the grid
    dat_in$norm <- norms
    dat_in$bt_norm <- btnorms
    
  }
  
  # exit function
  return(dat_in)
    
}