######
#' Get WRTDS trends using seasonal Kendall tests
#'
#' Get WRTDS trends using seasonal Kendall tests
#' 
#' @param dat_in input tidal or tidalmean object which must already have fitted model data
#' @param mobrks list of month groupings where each month is an integer from 1 to 12, see examples
#' @param yrbrks numeric vector of breaks for years, see examples
#' @param molabs character vector of names for month breaks, see examples
#' @param yrlabs character vector of names for year breaks, see examples
#' @param tau numeric vector of quantile for estimating trends
#' @param trndvar chr string of variable for trend evaluation, usually back-transformed, flow-normalized results, see details
#' @param ... methods passed to or from other methods
#' 
#' @export
#' 
#' @details Trends are based on \code{\link[EnvStats]{kendallSeasonalTrendTest}} for user-specified time periods.  In general, the seasonal Kendall test evaluates monotonic trends using a non-parametric approach that accounts for seasonal variation in the time series.  
#' 
#' All trends are based on back-transformed, flow-normalized results by default. The variable for evaluating trends can be changed with \code{'trndvar'} as \code{'res'}, \code{'norm'}, or \code{'fit'} for \code{tidal} objects and as \code{'res'}, \code{'bt_norm'}, or \code{'bt_fits'} for \code{tidalmean} objects.  In all cases, back-transformed variables are evaluated.
#' 
#' The user must supply the annual and monthly aggregation periods to the appropriate arguments. These are passed to \code{\link[base]{cut}} and are left-open, right-closed along the interval. 
#' 
#' @import dplyr
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @importFrom lubridate month year
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
#' 
#' @return A \code{\link[base]{data.frame}} with summary trends for each grouping, including \code{med} as the median value for the period of observation, \code{tau} as the magnitude and direction of the trend, \code{slope} as the Thiel-Sen slope for change per year, \code{chitest} as the signifiance test evaluating heterogeneity between seasons, \code{ztest} indicating significance of the overall trend, and \code{perchg} as 100 multiplied by the ratio of the annual slope to the median estimate of the time period (percent change per year).  
#' 
#' As noted in \code{\link[EnvStats]{kendallSeasonalTrendTest}}, the overall test is not appropriate if \code{chitest} indicates a small p-value.  
#' 
#' @references Hirsch, R.M., Slack, J.R., Smith, R.A. 1982. Techniques of trend analysis for monthly water quality data. Water Resources Research, 18:107-121.
#' 
#' Millard, S. P. 2013. EnvStats: An R Package for Environmental Statistics. Springer, New York. 
#' 
#' @examples
#' ## load a fitted model object
#' data(tidfit)
#' data(tidfitmean)
#' 
#' ## get trends
#' 
#' # setup month, year categories
#' mobrks <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(10, 11, 12))
#' yrbrks <- c(1973, 1985, 1994, 2003, 2012)
#' molabs <- c('JFM', 'AMJ', 'JAS', 'OND')
#' yrlabs <- c('1974-1985', '1986-1994', '1995-2003', '2004-2012')
#'
#' wrtdstrnd_sk(tidfit, mobrks, yrbrks, molabs, yrlabs)
#' wrtdstrnd_sk(tidfitmean, mobrks, yrbrks, molabs, yrlabs)
#' 
wrtdstrnd_sk <- function(dat_in, ...) UseMethod('wrtdstrnd_sk')
  
#' @rdname wrtdstrnd_sk
#'
#' @export
#'
#' @method wrtdstrnd_sk default
wrtdstrnd_sk.default <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, ...){

  # create year mo cats
  dat_in <- select(dat_in, date, trndvar) %>% 
    mutate(
      yr = year(date), 
      mo = month(date)
    )
  
  # annual aggs
  yrdat <- mutate(dat_in,
      yrcat = cut(yr, breaks = yrbrks, labels = yrlabs)
      ) %>% 
    na.omit %>% 
    rename(cat = yrcat) %>% 
    group_by(cat) %>% 
    nest %>% 
    mutate(
      sk_res = map(data, ~ kendallSeasonalTrendTest(trndvar ~ mo + yr, .)), 
      med = map(data, ~ median(.$trndvar, na.rm = TRUE)), 
      tau = map(sk_res, ~ .$estimate[1]), 
      slope = map(sk_res, ~ .$estimate[2]), 
      chitest = map(sk_res, ~ .$p.value[1]), 
      ztest = map(sk_res, ~ .$p.value[2])
    ) %>% 
    select(-data, -sk_res) %>% 
    unnest

  # setup monthly categories
  if(length(molabs) != length(mobrks))
    stop('molabs are not matched to mobrks')
  names(mobrks) <- molabs
  mobrks <- as.data.frame(mobrks) %>% 
    tidyr::gather(.) %>% 
    arrange(value)

  # monthly aggs
  modat <- mutate(dat_in, 
    mocat = factor(mo)
    ) 
  levels(modat$mocat) <- mobrks$key
  modat <- group_by(modat, yr, mo, mocat) %>% 
    summarise(trndvar = mean(trndvar, na.rm = T)) %>% 
    arrange(yr, mo) %>% 
    rename(cat = mocat) %>% 
    group_by(cat) %>% 
    nest %>% 
    mutate(
      sk_res = map(data, ~ kendallSeasonalTrendTest(trndvar ~ mo + yr, .)), 
      med = map(data, ~ median(.$trndvar, na.rm = TRUE)), 
      tau = map(sk_res, ~ .$estimate[1]), 
      slope = map(sk_res, ~ .$estimate[2]), 
      chitest = map(sk_res, ~ .$p.value[1]), 
      ztest = map(sk_res, ~ .$p.value[2])
    ) %>% 
    select(-data, -sk_res) %>% 
    unnest
  
  # combine results
  out <- rbind(yrdat, modat) %>% 
    ungroup %>% 
    mutate(
      cat = factor(cat, 
        levels = c(yrlabs, molabs),
        labels = c(yrlabs, molabs)
      ), 
      perchg = 100 * slope/med
    ) %>% 
    arrange(cat) %>% 
    data.frame
  
  return(out)
    
}

#' @rdname wrtdstrnd_sk
#' 
#' @export
#'
#' @method wrtdstrnd_sk tidal
wrtdstrnd_sk.tidal <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, tau = NULL, trndvar = 'norm', ...){

  # stop if trndvar not one of these
  if(!trndvar %in% c('res', 'norm', 'fit'))
    stop('var must be one of res, norm, or fit')
  
  # get names if trndvar isn't res
  if(trndvar != 'res'){
    
    # get tau if null, otherwise run checks
    if(is.null(tau)){
  
      fstr <- paste0('^', trndvar)
      tau <- grep(fstr, names(dat_in))
      tau<- gsub(fstr, '', names(dat_in)[floor(median(tau))])
      toget <- c('^date$', paste0(fstr, tau, '$'))       
      
    } else {
      
      if(length(grep(paste0(tau, '$'), names(dat_in))) == 0)
        stop('Specified tau not in object')
      
    }
    
  } else {
    
    toget <- c('^date$', paste0('^', trndvar, '$'))  
     
  }

  # columns to get with regex
  toget <- paste(toget, collapse = '|')

  # select columns, format date as year, month
  # back transform
  toeval <- select(dat_in, matches(toget))
  names(toeval) <- c('date', 'trndvar')
  toeval <- mutate(toeval,
      trndvar = exp(trndvar)
    )

  wrtdstrnd_sk.default(toeval, mobrks, yrbrks, molabs, yrlabs, ...) 
    
}

#' @rdname wrtdstrnd_sk
#' 
#' @export
#'
#' @method wrtdstrnd_sk tidalmean
wrtdstrnd_sk.tidalmean <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, trndvar = 'bt_norm', ...){

  # stop if trndvar not one of these
  if(!trndvar %in% c('res', 'bt_norm', 'bt_fits'))
    stop('var must be one of res, bt_norm, or bt_fits')
  
  # columns to get with regex
  toget <- paste0('^date$|^', trndvar, '$')

  # select columns, format date as year, month
  toeval <- select(dat_in, matches(toget))
  names(toeval) <- c('date', 'trndvar')

  # need to back-transform res if used
  if(trndvar == 'res')
    toeval <- mutate(toeval, 
      trndvar = exp(trndvar)
    )
  
  wrtdstrnd_sk.default(toeval, mobrks, yrbrks, molabs, yrlabs, ...) 
  
}
