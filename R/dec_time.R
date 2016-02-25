#' Create decimal time from time vector
#'
#' Create decimal time on an annual scale from an input time vector
#'
#' @param date_in input time vector, usually a \code{date} object
#' 
#' @details Function is used internally within the package.
#' 
#' @export
#' 
#' @return A named list of four numeric vecors including \code{day_num} (decimal day on an annual scale), \code{month} (month of the year as integer), \code{year}, and \code{dec_time} (decimal time as sum of \code{year} and \code{day_num})
#' 
#' @examples
#' dt <- Sys.Date()
#' dts <- seq.Date(dt - 365, dt, by = 'day') 
#' 
#' dec_time(dts)
dec_time <- function(date_in) UseMethod('dec_time')

#' @rdname dec_time
#'
#' @export
#'
#' @method dec_time Date
dec_time.Date <- function(date_in){

  # separate input date into year, month, day (as decimal by year)
  day_num <- as.numeric(strftime(date_in, '%j')) + 1
  year <- as.numeric(strftime(date_in, '%Y'))
  month <- as.numeric(strftime(date_in, '%m'))
  lp_days <- day_num %in% c(61, 92, 122, 153, 183, 214, 245, 275, 306, 336)
  day_num[lp_days] <- day_num[lp_days] - 1
  day_num <- day_num/365
  
  # decimal time as sum of year and day_num
  dec_time <- year + day_num
  
  # output
  out <- list(day_num = day_num, month = month, year = year, dec_time = dec_time)
  return(out)
  
}
