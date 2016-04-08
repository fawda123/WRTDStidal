######
#' Get WRTDS trends
#'
#' Get WRTDS trends for annual and monthly groupings
#' 
#' @param dat_in input tidal or tidalmean object which must already have fitted model data
#' @param mobrks numeric vector of breaks for months, see examples
#' @param yrbrks numeric vector of breaks for years, see examples
#' @param molabs character vector of names for month breaks, see examples
#' @param yrlabs character vector of names for year breaks, see examples
#' @param tau numeric vector of quantile for estimating trends
#' @param ... methods passed to or from other methods
#' 
#' @export
#' 
#' @details Trends are reported as percent changes of annual averages from the beginning to the end of each period.  To reduce the effects of odd years at the beginning and end of each period, percent changes are based on average of the first three and last three annaul averages.  For example, percent changes for January throughout an an entire time series from 1980 to 2000 would be the change of the average from January in 1980-1982 to the average from January in 1998-2000.  Annual trends, e.g., percent changes from 1980-1986, 1987-1993, etc. do not average by the first and last three years in each grouping because the values are already based on annual averages.  
#' 
#' All trends are based on back-transformed, flow-normalized results. 
#' 
#' The user must supply the annual and monthly aggregation periods to the appropriate arguments. These are passed to \code{\link[base]{cut}} and are left-open, right-closed along the interval. 
#' 
#' @import dplyr
#' @importFrom lubridate month year
#' 
#' @return A \code{\link[base]{data.frame}} with summary trends for each grouping
#' 
#' @examples
#' ## load a fitted model object
#' data(tidfit)
#' data(tidfitmean)
#' 
#' ## get trends
#' 
#' # setup month, year categories
#' mobrks <- c(-Inf, 3, 6, 9, Inf)
#' yrbrks <- c(-Inf, 1985, 1994, 2003, Inf)
#' molabs <- c('JFM', 'AMJ', 'JAS', 'OND')
#' yrlabs <- c('1974-1985', '1986-1994', '1995-2003', '2004-2012')
#'
#' wrtdstrnd(tidfit, mobrks, yrbrks, molabs, yrlabs)
#' wrtdstrnd(tidfitmean, mobrks, yrbrks, molabs, yrlabs)
wrtdstrnd <- function(dat_in, ...) UseMethod('wrtdstrnd')
  
#' @rdname wrtdstrnd
#'
#' @export
#'
#' @method wrtdstrnd default
wrtdstrnd.default <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, ...){

  # aggregate/summarize separately for each category
  
  # annual aggs
  yrdat <- group_by(dat_in, date, yrcat) %>% 
    summarise(norm = mean(norm, na.rm = T)) %>% 
    rename(cat = yrcat) %>% 
    group_by(cat) %>% 
    summarise(
      chg = chngest(date, norm, single = T) # important!
    )
  
  # monthly aggs
  modat <- group_by(dat_in, date, mocat) %>% 
    summarise(norm = mean(norm, na.rm = T)) %>% 
    rename(cat = mocat) %>% 
    group_by(cat) %>% 
    summarise(chg = chngest(date, norm))
  
  # combine
  out <- rbind(yrdat, modat) %>% 
    ungroup %>% 
    mutate(
      cat = factor(cat, 
        levels = c(yrlabs, molabs),
        labels = c(yrlabs, molabs)
      )
    ) %>% 
    arrange(cat) %>% 
    data.frame
  
  return(out)
    
}

#' @rdname wrtdstrnd
#' 
#' @export
#'
#' @method wrtdstrnd tidal
wrtdstrnd.tidal <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, tau = NULL, ...){

  # get tau if null, otherwise run checks
  if(is.null(tau)){

    tau <- grep('^norm', names(dat_in))
    tau<- gsub('^norm', '', names(dat_in)[floor(median(tau))])
     
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(dat_in))) == 0)
      stop('Specified tau not in object')
    
  }

  # columns to get with regex
  toget <- c('^date$', paste0('^norm', tau, '$'))
  toget <- paste(toget, collapse = '|')

  # select columns, format date as year, month
  toeval <- select(dat_in, matches(toget))
  names(toeval) <- c('date', 'norm')
  toeval <- mutate(toeval,
      yrcat = cut(year(date), breaks = yrbrks, labels = yrlabs),
      mocat = cut(month(date), breaks = mobrks, labels = molabs),
      date = year(date),
      norm = exp(norm)
    )

  wrtdstrnd.default(toeval, mobrks, yrbrks, molabs, yrlabs) 
    
}

#' @rdname wrtdstrnd
#' 
#' @export
#'
#' @method wrtdstrnd tidalmean
wrtdstrnd.tidalmean <- function(dat_in, mobrks, yrbrks, molabs, yrlabs, ...){

  # columns to get with regex
  toget <- c('^date$|^bt_norm$')

  # select columns, format date as year, month
  toeval <- select(dat_in, matches(toget))
  names(toeval) <- c('date', 'norm')
  toeval <- mutate(toeval,
      yrcat = cut(year(date), breaks = yrbrks, labels = yrlabs),
      mocat = cut(month(date), breaks = mobrks, labels = molabs),
      date = year(date)
    )
 
    wrtdstrnd.default(toeval, mobrks, yrbrks, molabs, yrlabs) 
  
}
