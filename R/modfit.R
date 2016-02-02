######
#' Fit weighted regression and get predicted/normalized response variable
#'
#' Fit weighted regression and get predicted/normalized response variable from a data frame.  This is a wrapper for multiple function used to create a weighted regression model and should be used rather than the individual functions.
#'
#' @param dat_in input \code{\link[base]{data.frame}} for fitting the model, see details
#' @param resp_type chr string indicating the type of model response to use, quantile or mean model
#' @param ... arguments passed to or from other methods
#' 
#' @export
#' 
#' @return A tidal object with predicted and normalized response variable predictions, attributes updated accordingly.
#' 
#' @details This function is used as a convenience to combine several functions that accomplish specific tasks, primarily the creation of a tidal or tidalmean object, fitting of the weighted regression models with \code{\link{wrtds}}, extraction of fitted values from the interpolation grids using \code{\link{respred}}, and normalization of the fitted values from the interpolation grid using \code{\link{resnorm}}.  The format of the input should be a \code{\link[base]{data.frame}} with response variable observations as rows and the first four columns as date, response variable, salinity/flow, and detection limits.  The order of the columns may vary provided the order of each of the four critical variables is specified by the \code{ind} argument that is passed to the \code{\link{tidal}} or \code{\link{tidalmean}} function.  The response variable data are also assumed to be in log-space, otherwise use \code{reslog = FALSE} which is also passed to the \code{\link{tidal}} or \code{\link{tidalmean}} function.  The dataset described in \code{\link{chldat}} is an example of the correct format.  
#' 
#' For quantile models, the default conditional quantile that is predicted is the median (\code{tau = 0.5}, passed to the \code{\link{wrtds}} function).  Numerous other arguments affect the output and the default parameters may not be appropriate for all scenarios.  Arguments used by other functions can be specified explicitly with the initial call.  The documentation for the functions under `see also' should be consulted for available arguments, as well as the examples that illustrate common changes to the default values.
#'
#' @seealso See the help files for \code{\link{tidal}}, \code{\link{tidalmean}}, \code{\link{wrtds}}, \code{\link{getwts}}, \code{\link{respred}}, and \code{\link{resnorm}} for arguments that can be passed to this function.
#'
#' @examples
#' \dontrun{
#' ## load data
#' data(chldat)
#' 
#' ## fit the model and get predicted/normalized data for response variable
#' # default median fit
#' # grids predicted across salinity range with ten values
#' res <- modfit(chldat)
#' 
#' # for mean models
#' res <- modfit(chldat, resp_type = 'mean')
#' 
#' ## fit different quantiles and smaller interpolation grid
#' res <- modfit(chldat, tau = c(0.2, 0.8), flo_div = 5)
#' 
#' ## fit with different window widths
#' # half-window widths of one day, five years, and 0.3 salinity
#' res <- modfit(chldat, wins = list(1, 5, 0.3))
#' 
#' ## suppress console output
#' res <- modfit(chldat, trace = FALSE)
#' }
modfit <- function(dat_in, ...) UseMethod('modfit')

#' @rdname modfit
#'
#' @export
#'
#' @method modfit data.frame
modfit.data.frame <- function(dat_in, resp_type = 'quantile', ...){
  
  # sanity check
  if(!grepl('quantile|mean', resp_type))
    stop('Response type must be quantile or mean')
  
  if(resp_type == 'quantile'){
    # append data to arguments, create tidal object
    args <- c(list(dat_in = dat_in), list(...))
    dat <- do.call(tidal, args)
  }
  
  if(resp_type == 'mean'){
    # append data to arguments, create tidalmean object
    args <- c(list(dat_in = dat_in), list(...))
    dat <- do.call(tidalmean, args)
  }
    
  # update args, get interpolation grids
  args <- c(list(dat_in = dat, all = F), list(...))
  dat <- do.call(wrtds, args)
  
  # update args, get predictions
  args[['dat_in']] <- dat
  dat <- do.call(respred, args)
  
  # get normalized predictions
  args[['dat_in']] <- dat
  dat <- do.call(resnorm, args)
  
  # return output
  return(dat)
  
}

