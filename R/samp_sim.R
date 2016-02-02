#' Sample a daily time series at a set frequency
#'
#' Sample a daily water quality time series at a set monthly frequency
#'
#' @param dat_in input \code{\link[base]{data.frame}} that is returned from \code{\link{lnres_sim}} or \code{\link{all_sims}}
#' @param month_samps numeric indicating sampling interval, defaults to once a month and is random within the month.  Multiple samples per month are based on sampling with replacement. 
#' 
#' @details This function is intended for sampling a simulated daily time series of water quality that is returned by \code{\link{lnres_sim}} or \code{\link{all_sims}}.
#' 
#' @return Original data frame with rows subset based on number of desired monthly samples.
#' 
#' @export
#' 
#' @seealso \code{\link{lnres_sim}}, \code{\link{all_sims}}
#' 
#' @examples
#' \dontrun{
#' ## example data
#' data(daydat)
#' 
#' ## simulate
#' tosamp <- all_sims(daydat)
#' 
#' ## sample
#' samp_sim(tosamp)
#' }
samp_sim <- function(dat_in, month_samps = 1){
 
  dat_in$mos <- strftime(dat_in$date, '%m')

  splits <- split(dat_in, dat_in[, c('year', 'mos')])
  
  sels <- lapply(splits, function(x){
    tosel <- sample(1:nrow(x), size = month_samps, replace = TRUE)
    x[tosel, ]
  })

  sels <- do.call('rbind', sels)
  sels <- sels[order(sels$date), ]
  row.names(sels) <- 1:nrow(sels)
  sels$mos <- NULL

  return(sels)
   
}
