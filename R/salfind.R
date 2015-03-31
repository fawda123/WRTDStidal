######
#' Get all salinity values across years for a common date
#' 
#' Get all salinity values across years for a common date for normalizing chlorophyll predictions
#'
#' @param tidal_in input tidal object
#' @param obs index of row in tidal object to identify salinity values
#' @param ... arguments passed to additional methods
#' 
#' @return Numeric vector of all observed salinity values for a single month across all years
#' 
#' @details This function is primarily used within \code{\link{chlnorm}}.  It will return all observed salinity values for a given date for matching months across all years.  For example, the function will return all observed salinity values for January across all years in the dataset if the input row is for January 2000.  An identical vector would result for the same month in other years.
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
salfind <- function(tidal_in, ...) UseMethod('salfind')

#' @rdname salfind
#'
#' @export
#'
#' @method salfind tidal
salfind.tidal <- function(tidal_in, obs, ...){
  
  # get observation of interest
  to_find <- tidal_in[obs, ]
  
  # logical vector indicating matching months
  mo_match <- tidal_in$month %in% to_find$month
  
  # get matching salinity values
  sal_match <- tidal_in[mo_match, 'sal']
  
  # return output
  return(sal_match)

}