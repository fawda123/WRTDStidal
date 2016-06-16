#' Create annual aggregations of WRTDS output
#'
#' Create annual aggregations of WRTDS output
#' 
#' @param dat_in input tidal or tidalmean object
#' @param mo_strt numeric indicating month to start aggregation years, defaults to October for USGS water year from October to September
#' @param min_mo numeric value from one to twelve indicating the minimum number of months with observations for averaging by years, applies only if \code{annuals = TRUE}
#' @param ... arguments passed to or from other methods
#' 
#' @details WRTDS output is averaged by year for both predictions and flow-normalized predictions. Years are averaged only if one observation is contained in each of the minimum number of months specified by \code{min_mo} averaging, otherwise results are not returned for the given year.  Note that setting \code{min_mo} to values smaller than the default can produce inaccurate trends for years with very few results. 
#'
#' The function is used internally within \code{\link{prdnrmplot}} and \code{\link{fitplot}}.
#' 
#' @export
#' 
#' @import dplyr
#' 
#' @return An aggregated data object for plotting, returns only model output and response variable.  
#' 
#' @examples
#' ## tidal object
#' annual_agg(tidfit)
#'
#' ## tidalmean object
#' annual_agg(tidfitmean)
annual_agg <- function(dat_in, ...) UseMethod('annual_agg')

#' @rdname annual_agg
#'
#' @export
#'
#' @method annual_agg default
annual_agg.default <- function(dat_in, mo_strt = 10, min_mo = 9,  ...){
  
  # put data in long format by response, fits, norms
  # split by response variable
  # create new year column based on mo_strt
  # average by new year column only if minimum number of months are present
  dat_agg <- select(dat_in, matches('date|fit|norm|res')) %>% 
    tidyr::gather('var', 'val', -date) %>% 
    split(.$var) %>% 
    lapply(., function(x){

      out <- na.omit(x) %>% 
        mutate(
          yr = lubridate::year(date),
          mo = lubridate::month(date)
        ) %>% 
        mutate(
          yrchg = ifelse(mo < mo_strt, yr, yr + 1)
        ) %>% 
        group_by(yrchg, var) %>% 
        summarise(val = ifelse(length(unique(mo)) < min_mo, NA, mean(val)))
        
      out
      
    }) %>% 
    do.call('rbind', .) %>% 
    tidyr::spread(var, val) %>%
    ungroup %>% 
    rename(date = yrchg) %>% 
    mutate(date = as.Date(paste0(mo_strt, '-01-', date), format = '%m-%d-%Y'))
  
  return(dat_agg)
  
}