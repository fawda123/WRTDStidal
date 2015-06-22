######
#' Get all salinity values across years for a common month
#' 
#' Get all salinity values across years for a common month for normalizing chlorophyll predictions
#'
#' @param dat_in input tidal object
#' @param obs index of row in tidal object to identify salinity values
#' @param ... arguments passed to additional methods
#' 
#' @return Numeric vector of all observed salinity values for a single month across all years
#' 
#' @details This function is used in \code{\link{chlnorm}}.  It will return all observed salinity values for a given date for matching months across all years.  For example, the function will return all observed salinity values for January across all years in the dataset if the input row is for January 2000.  An identical vector would result for the same month in other years.  Salinity values are averaged when more than one observation was taken in the same month for a given year.
#' 
#' @export
#' 
#' @examples
#' ##
#' 
#' # load data
#' data(tidobj)
#' 
#' # find common salinity values across years for the first observation
#' res <- salfind(tidobj, 1)
salfind <- function(dat_in, ...) UseMethod('salfind')

#' @rdname salfind
#'
#' @export
#'
#' @method salfind default
salfind.default <- function(dat_in, obs, ...){
  
  # get observation of interest
  to_find <- dat_in[obs, ]
  
  # logical vector indicating matching months
  mo_match <- dat_in$month %in% to_find$month
  
  # get matching salinity values
  sal_match <- dat_in[mo_match, c('year', 'sal')]
  sal_match <- aggregate(sal ~ year, sal_match, FUN = mean, na.rm = TRUE)
  
  # return output
  return(sal_match$sal)

}