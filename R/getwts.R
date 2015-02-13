######
#' Get weights for regression
#' 
#' Get weights for regression for a single observation using a tri-cubic weighting function
#' 
#' @param tidal_in input tidal object
#' 
#' @return Some weights
#' 
#' @export
#' 
#' @examples
#' ##
getwts <- function(tidal_in) UseMethods('getwts')

#' @rdname getwts
#' 
#' @param ref_in row of tidal object as reference for weights
#' @param wins list of half-window widths for time, year, and salinity
#' @param all logical to return individual weights rather than the product of all three, default \code{FALSE}
#' @param min_obs logical to use window widening if less than 100 non-zero weights are found, default \code{TRUE}
#'
#' @export
#' 
#' @method getwts tidal
getwts.tidal <- function(tidal_in, ref_in,
  wins = list(0.5, 10, NULL),
  all = F,
  min_obs = T){
  
  #sanity check
  if(sum(wt_vars %in% names(dat_in)) != length(wt_vars))
    stop('Weighting variables must be named in "dat_in"')
  
  #windows for each of three variables
  wins_1 <- wins[[1]]
  wins_2 <- wins[[2]]
  wins_3 <- wins[[3]]
  
  if(is.null(wins[[3]])) wins_3 <- diff(range(dat_in[, wt_vars[3]]))/2
  
  #weighting tri-cube function
  wt_fun_sub <- function(dat_cal, ref, win, mo = F){
    
    dist_val <- abs(dat_cal-ref)
    
    if(mo){
      dist_val <- pmin(
        ref + 1 - dat_cal,
        dat_cal + 1 - ref,
        dist_val
        )
      }
    
    if(dist_val <= win) return((1 - (dist_val/win)^3)^3)
    
    else return(0)
      
    }

  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])

  #weights for each observation in relation to reference
  wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, ref = ref_1, win = wins_1, mo = T)
  wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, ref = ref_2, win = wins_2)
  wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, ref = ref_3, win = wins_3)
  out <- wts_1 * wts_2 * wts_3
  
  gr_zero <- sum(out>0)
  #cat('   Number of weights greater than zero =', gr_zero, '\n')
  
  while(gr_zero < 100){
    
    wins_1 <- 0.1 * wins_1 + wins_1
    wins_2 <- 0.1 * wins_2 + wins_2
    wins_3 <- 0.1 * wins_3 + wins_3
    
    #weights for each observation in relation to reference
    wts_1 <- sapply(as.numeric(dat_in[, wt_vars[1]]), wt_fun_sub, ref = ref_1, win = wins_1, mo = T)
    wts_2 <- sapply(as.numeric(dat_in[, wt_vars[2]]), wt_fun_sub, ref = ref_2, win = wins_2)
    wts_3 <- sapply(as.numeric(dat_in[, wt_vars[3]]), wt_fun_sub, ref = ref_3, win = wins_3)
    
    out <- wts_1 * wts_2 * wts_3
    
    gr_zero <- sum(out > 0)
    
    #cat('   Number of weights greater than zero', gr_zero, '\n')
    
    }
  
  #return all weights if T
  if(all){
    out <- data.frame(wts_1, wts_2, wts_3)
    names(out) <- wt_vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }