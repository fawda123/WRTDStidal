######
#' Get weights for regression
#' 
#' Get weights for WRTDS for a single observation using a tri-cubic weighting function
#' 
#' @param dat_in input tidal object
#' @param ref_in row of tidal object as reference for weights
#' @param wt_vars chr string of three elements indicatings names of columns in tidal object that are used for reference row weights
#' @param wins list of half-window widths for time, year, and flow
#' @param all logical to return individual weights rather than the product of all three, default \code{FALSE}
#' @param slice logical indicating if data are subset by observations within the maximum window width for faster calculations
#' @param ngrzero logical indicating if count of observations with weights greater than zero is returned
#' @param wins_only logical if the half-window widths should be returned as a list
#' @param min_obs numeric vector for window widening if the number of observations with non-zero weights is less than the specified value, use \code{min_obs = NULL} to suppress this behavior
#' @param ... arguments passed to or from other methods
#' 
#' @return A vector of weights with length equal to the number of observations (rows) in the tidal object.  Vectors for all three weighting variables are returned if \code{all = TRUE}.
#' 
#' @details
#' The default half-window widths for \code{day_num}, \code{year}, and \code{flow} are half a day (12 hours), 10 years, and half the range of salinity/flow in the input data.  The half-window widths are expanded by 10\% until at least 100 observations have weights greater than zero.  This behavior can be suppressed by setting \code{min_obs = NULL}.
#' 
#' @export
#' 
#' @examples
#' ##
#' data(tidobj)
#' 
#' # get weights for first row
#' first <- tidobj[1, ]
#' wts <- getwts(tidobj, first)
#' 
#' plot(wts, type = 'l')
#' 
#' \dontrun{
#' 
#' # get count of observations with grzero weights
#' sapply(1:nrow(tidobj), function(row) getwts(tidobj, tidobj[row, ], 
#'  ngrzero = TRUE))
#' }
getwts <- function(dat_in, ...) UseMethod('getwts')

#' @rdname getwts
#'
#' @export
#' 
#' @method getwts default
getwts.default <- function(dat_in, ref_in,
  wt_vars = c('day_num', 'dec_time', 'flo'),
  wins = list(0.5, 10, NULL),
  all = FALSE, 
  slice = TRUE, 
  ngrzero = FALSE, 
  wins_only = FALSE,
  min_obs = 100, ...){
  
  # sanity check
  if(sum(wt_vars %in% names(dat_in)) != length(wt_vars))
    stop('Weighting variables must be named in "dat_in"')
  
  # windows for each of three variables
  wins_1 <- wins[[1]]
  wins_2 <- wins[[2]]
  wins_3 <- wins[[3]]
  
  # default window width for third variable is half its range
  if(is.null(wins[[3]])) wins_3 <- diff(range(dat_in[, wt_vars[3]], na.rm = TRUE))/2
  
  # return windows if T, for tidal attributes
  if(wins_only) return(list(wins_1, wins_2, wins_3))
  
  # weighting tri-cube function
  # mirror extends weighting function if vector repeats, e.g. monthly
  # 'dat_cal' is observation for weight assignment
  # 'ref' is reference observation for fitting the model
  # 'win' is window width from above (divided by two)
  # 'mirr' is logical indicating if distance accounts for repeating variables (e.g., month)
  # 'scl_val' is range for the ref vector of obs, used to get correct distance for mirrored obs
  wt_fun_sub <- function(dat_cal, ref, win, mirr = F, scl_val = 1){
    
    # dist_val is distance of value from the ref
    dist_val <- abs(dat_cal - ref)
    
    # repeat if distance is checked on non-continuous number line
    if(mirr){
      
      dist_val <- pmin(
          abs(ref + scl_val - dat_cal),
          abs(dat_cal + scl_val - ref),
          dist_val
          )
      
      }
    
    # get wts within window, otherwise zero
    win_out <- dist_val > win
    dist_val <- (1 - (dist_val/win)^3)^3
    dist_val[win_out] <- 0
      
    return(dist_val)
      
    }

  #reference (starting) data
  ref_1 <- as.numeric(ref_in[, wt_vars[1]])
  ref_2 <- as.numeric(ref_in[, wt_vars[2]])
  ref_3 <- as.numeric(ref_in[, wt_vars[3]])

  ##
  # subset 'dat_in' by max window size for faster calc
  # this is repeated if min number of wts > 0 is not met
  # subset vector is all T if not using subset
  yr_rng <- range(dat_in$year)
  ref_rng <- unique(ref_in$year)
  dec_sub <- with(dat_in, 
    year > ref_rng - wins_2 * 1.1 & year < ref_rng + wins_2 * 1.1
    )
  if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
  dat_sub <- dat_in[dec_sub, ]

  ##
  # weights for each observation in relation to reference
  # see comments for 'wt_fun_sub' for 'scl_val' argument
  
  # jday
  wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
    ref = ref_1, win = wins_1, mirr = T) 
  # hour
  wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
    ref = ref_2, win = wins_2, mirr = F)
  # tide
  wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
    ref = ref_3, win = wins_3, mirr = F)
  # all as product 
  out <- wts_1 * wts_2 * wts_3
  
  gr_zero <- sum(out > 0, na.rm = TRUE)
  
  # return count of wts greater than zero if T
  if(ngrzero) return(gr_zero)
  
  # extend window widths of weight vector is less than 100
  if(!is.null(min_obs)){
    
    # stop if insufficient sample size
    if(nrow(dat_in) < min_obs)
      stop('min_obs argument greater than sample size')

    while(any(gr_zero < min_obs)){
      
      # increase window size by 10%
      wins_1 <- 1.1 * wins_1
      wins_2 <- 1.1 * wins_2
      wins_3 <- 1.1 * wins_3 
      
      # subset again
      dec_sub <- with(dat_in, 
        year > ref_rng - wins_2 * 1.1 & year < ref_rng + wins_2 * 1.1
        )
      if(!slice) dec_sub <- rep(T, length = nrow(dat_in))
      dat_sub <- dat_in[dec_sub, ]
      
      #weights for each observation in relation to reference
      wts_1 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[1]]), 
        ref = ref_1, win = wins_1, mirr = T)
      wts_2 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[2]]), 
        ref = ref_2, win = wins_2, mirr = F, scl_val = 24)
      wts_3 <- wt_fun_sub(as.numeric(dat_sub[, wt_vars[3]]), 
        ref = ref_3, win = wins_3, mirr = F)
      
      out <- wts_1 * wts_2 * wts_3
      
      gr_zero <- sum(out > 0, na.rm = TRUE)
      
    }
  }
  
  # extend weight vectors to length of dat_in
  empty_mat <- matrix(0, ncol = nrow(ref_in), nrow = nrow(dat_in))
  empty_fill <- function(wts_in) {
    out <- empty_mat
    out[dec_sub, ] <- wts_in
    out
    }
  wts_1 <- empty_fill(wts_1)
  wts_2 <- empty_fill(wts_2)
  wts_3 <- empty_fill(wts_3)  
  out <- c(empty_fill(out))

  #return all weights if T
  if(all){
    out <- data.frame(wts_1, wts_2, wts_3)
    names(out) <- wt_vars
    return(out)
    }
  
  #final weights are product of all three
  out
  
  }
