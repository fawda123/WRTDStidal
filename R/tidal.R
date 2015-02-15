######
#' Create a tidal class object
#' 
#' Prepare water quality data for weighted regression by creating a tidal class object
#' 
#' @param dat_in Input data frame for a water quality time series with three columns for date (Y-m-d format), chlorophyll concentation (ug/L), and fraction of freshwater
#' @param four element numeric vector indicating column positions of date, chlorophyll, salinity, and detection limit of input data frame
#' @param chllog logical indicating if input chlorophyll is already in log-space, default \code{TRUE}
#' 
#' @return A \code{data.frame} with columns ordered as date, chlorophyll, salinity, and decimal time
#'
#' @details
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used to create a tidal object for use with weighted regression in tidal waters. Input data should be a three-column \code{\link[base]{data.frame}} with date, chlorophyll, salinity data, and detection limit for each chlorophyll observation.  Chlorophyll data are assumed to be log-transformed, otherwise use \code{chllog = FALSE}.  Salinity data should be represented as fraction of freshwater.  The limit column can be entered as a sufficiently small number if all values are above the detection limit or no limit exists.  
#'  
#' @export
#' 
#' @examples
#' ## raw data
#' 
#' data(chldat)
#' 
#' ## format
#' chldat <- tidal(chldat)
#' 
tidal <- function(dat_in, ind = c(1, 2, 3, 4), chllog = TRUE){
  
  # sanity checks
  if(!any(c('Date', 'POSIXct', 'POSIXlt') %in% class(dat_in[, ind[1]])))
    stop('Class for time column must be Date, POSIXct, or POSIXlt')
  
  # get relevant columns and set names
  dat_in <- dat_in[, ind, drop = F]
  names(dat_in) <- c('Date', 'chla', 'salff', 'lim')
  
  # log transform chl if T
  if(!chllog) dat_in$chla <- log(dat_in$chla)
  
  # TF column of limits for surv regression
  dat_in$not_cens <- with(dat_in, chla > lim)
  
  # get decimal time
  day_num <- as.numeric(strftime(dat_in$Date, '%j')) + 1
  year <- as.numeric(strftime(dat_in$Date, '%Y'))
  lp_days <- day_num %in% c(61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  day_num[lp_days] <- day_num[lp_days] - 1
  day_num <- day_num/365
  dat_in$day_num <- day_num
  dat_in$year <- year
  dat_in$dec_time <- year + day_num
  
  # create class, with multiple attributes
  tidal <- structure(
    .Data = dat_in, 
    class = c('tidal', 'data.frame'),
    fits = NULL, 
    betas = NULL
    )
  
  return(tidal)
  
}
