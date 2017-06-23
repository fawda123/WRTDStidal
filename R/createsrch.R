#' Create a grid of half-window widths to evaluate
#' 
#' Create a grid of all unique combinations of half-window widths to evaluate.  The result can be passed to \code{\link{winsrch_grid}}.
#' 
#' @param mos numeric vector of half-window widths for months, a value of one indicates twelve months
#' @param yrs numeric vector of half-window widths for years, a value of one indicates one-year
#' @param flo numeric vector of half-window widths for salinity or flow, a value of one indicates the full range of values (100 percent)
#' 
#' @details The weighting function uses a tri-cube weighting scheme such that weights diminish with distance from the center of the window.  For example, a value of one for the month window does not mean that all months are weighted equally even though the window covers an entire calendar year.  
#' 
#' @return A matrix with number of rows equal to the product of the lengths of each input vector, where each row is a unique combination for the selected half-window widths. 
#' 
#' @export
#' 
#' @seealso \code{\link{winsrch_grid}}
#' 
#' @examples 
#' createsrch()
#' createsrch(1, 1, 1)
createsrch <- function(mos = c(seq(0.5, 1, by = 0.25), 2, 10), 
  yrs = c(seq(5, 15, by = 3), 50), flo = c(seq(0.5, 1, by = 0.1), 5)){
  
  grd <- expand.grid(mos, yrs, flo)
  names(grd) <- c('mos', 'yrs', 'flo')
  
  return(grd)

}
