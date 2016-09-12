#' Sample a daily time series at a set frequency
#'
#' Sample a daily water quality time series at a set monthly frequency
#'
#' @param dat_in input \code{\link[base]{data.frame}} that is returned from \code{\link{lnres_sim}} or \code{\link{all_sims}}
#' @param unit chr string indicating sampling unit, must be year, quarter, month, or week for equivalent lubridate function
#' @param irregular logical indicating if monthly sampling is done randomly within each \code{unit}, otherwise the first value is returned
#' 
#' @details This function is intended for sampling a simulated daily time series of water quality that is returned by \code{\link{lnres_sim}} or \code{\link{all_sims}}.
#' 
#' @return Original data frame with rows subset based on number of desired monthly samples.
#' 
#' @export
#' 
#' @import dplyr
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
samp_sim <- function(dat_in, unit = 'month', irregular = TRUE){

  # sanity check
  if(!unit %in% c('year', 'quarter', 'month', 'week'))
    stop('unit must year, quarter, month, or week')
  
  # get sampling unit
  uni_str <- paste0('lubridate::', unit, '(dat_in$date)')
  unit <- eval(parse(text = uni_str))
  
  # get indices in dat_in for sampling
  inds <- data.frame(inds = 1:nrow(dat_in), year = lubridate::year(dat_in$date), unit = unit) %>% 
    group_by(year, unit) %>% 
    summarize(inds = ifelse(irregular, sample(inds, 1), inds[1])) %>% 
    .$inds
  
  # output
  out <- dat_in[inds, ]
  return(out)
   
}
