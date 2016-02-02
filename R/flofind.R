######
#' Get all salinity or flow values across years for a common month
#' 
#' Get all salinity or flow values across years for a common month for normalizing predictions of the response variable
#'
#' @param dat_in input tidal object
#' @param obs index of row in tidal object to identify salinity or flow values
#' @param ... arguments passed to additional methods
#' 
#' @return Numeric vector of all observed salinity or flow values for a single month across all years
#' 
#' @details This function is used in \code{\link{resnorm}}.  It will return all observed salinity/flow values for a given date for matching months across all years.  For example, the function will return all observed salinity/flow values for January across all years in the dataset if the input row is for January 2000.  An identical vector would result for the same month in other years.  Salinity/flow values are averaged when more than one observation was taken in the same month for a given year.
#' 
#' @export
#' 
#' @examples
#' ##
#' 
#' # load data
#' data(tidobj)
#' 
#' # find common salinity/flow values across years for the first observation
#' res <- flofind(tidobj, 1)
flofind <- function(dat_in, ...) UseMethod('flofind')

#' @rdname flofind
#'
#' @export
#'
#' @method flofind default
flofind.default <- function(dat_in, obs, ...){
  
  # get observation of interest
  to_find <- dat_in[obs, ]
  
  # logical vector indicating matching months
  mo_match <- dat_in$month %in% to_find$month
  
  # get matching salinity/flow values
  flo_match <- dat_in[mo_match, c('year', 'flo')]
  flo_match <- aggregate(flo ~ year, flo_match, FUN = mean, na.rm = TRUE)
  
  # return output
  return(flo_match$flo)

}