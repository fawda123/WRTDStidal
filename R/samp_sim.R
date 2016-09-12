#' Sample a daily time series at a set frequency
#'
#' Sample a daily water quality time series at a set monthly frequency
#'
#' @param dat_in input \code{\link[base]{data.frame}} that is returned from \code{\link{lnres_sim}} or \code{\link{all_sims}}
#' @param unit chr string indicating sampling unit, must be year, quarter, month, week, or yday for equivalent lubridate function
#' @param irregular logical indicating if monthly sampling is done randomly within each \code{unit}, otherwise the first value is returned
#' @param missper numeric from 0-1 indicating percentage of observations used for test dataset
#' @param blck numeric indicating block size for resampling test dataset, see details
#' 
#' @details This function is intended for sampling a simulated daily time series of water quality that is returned by \code{\link{lnres_sim}} or \code{\link{all_sims}}.
#' 
#' The \code{missper} argument is used to create a test dataset as a proportion of all observations in the sub-sampled output dataset.  The test dataset is created with random block sampling appropriate for time series.  Block sampling of the output dataset occurs until the number of unique observations is equal to the percentage defined by \code{missper}.  Overlap of blocks are not doubly considered towards the observation counts to satisfy \code{missper}, i.e., sets of continuous observations longer than \code{blck} can be returned because of sampling overlap.  Setting \code{blck = 1} is completely random sampling.
#' 
#' @return Original data frame with rows subset based on number of desired monthly samples.  If \code{missper > 0}, a list is returned where the first element is the index values for the test dataset and the second is the complete subsampled dataset. 
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
#' 
#' ## sample and create test dataset
#' # test dataset is 30% size of monthly subample using block sampling with size = 4
#' samp_sim(tosamp, missper = 0.3, blck = 4)
#' }
samp_sim <- function(dat_in, unit = 'month', irregular = TRUE, missper = 0, blck = 1){

  # sanity check
  if(!unit %in% c('year', 'quarter', 'month', 'week', 'yday'))
    stop('unit must year, quarter, month, week,  or yday')

  # get sampling unit
  uni_str <- paste0('lubridate::', unit, '(dat_in$date)')
  units <- eval(parse(text = uni_str))
  
  # get indices in dat_in for sampling, only if not by day
  if(unit != 'yday'){
    inds <- data.frame(inds = 1:nrow(dat_in), year = lubridate::year(dat_in$date), units = units) %>% 
      group_by(year, units) %>% 
      summarize(inds = ifelse(irregular, sample(inds, 1), inds[1])) %>% 
      .$inds
  } else {
    inds <- 1:nrow(dat_in)
  }

  # output
  out <- dat_in[inds, ]
  
  # create missing data with random block sampling if missing percentage greater than zero
  if(missper > 0){
    
    # sanity checks
    if(missper > 1)
      stop('missper must be between 0 - 1')
    if(blck < 1)
      stop('block must be at least one')
      
    # number of samples to get and sample pool
    torm <- round(nrow(out) * missper)
    pool <- 1:nrow(out)
    
    # vectorized sampling for blk = 1
    if(blck == 1){
      
      smps <- sample(pool, torm, replace = FALSE) %>% 
        sort

    # otherwise start creating blocks
    } else {

      # initial grab
      blck_sd <- floor(torm/blck)
      smps <- sample(pool, blck_sd, replace = F) %>% 
        sapply(function(x) x:(x + blck - 1)) %>% 
        c %>% 
        unique %>% 
        .[. <= nrow(out)] %>% 
        sort

      # adjust sampling pool and number of samples left 
      pool <- pool[!pool %in% smps]
      lft <- torm - length(smps)
      
      # continue sampling one block at a time until enough samples in missper
      while(lft > 0){

        # take one sample with block size as minimum between block of samples left
        smps_tmp <- sample(pool, 1, replace = F) %>% 
              .:(. + pmin(lft, blck) - 1) 
   
        # append new sample to initial grab
        smps <- c(smps, smps_tmp) %>% 
          unique %>% 
          sort %>% 
          .[. <= nrow(out)]
        
        # update samples left and sample pool
        lft <- torm - length(smps)
        pool <- pool[!pool %in% smps]
        
      }
      
    }

    # out is now a list with block samples and subsampled data
    out <- list(smps = smps, alldat = out)
    
  }
    
  return(out)
   
}
