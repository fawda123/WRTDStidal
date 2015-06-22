#' Fill missing values in the interpolation grid
#' 
#' Fill missing values in the interpolation grid, only relevant for quantile models that describe the tails of the distribution.  Date, year, month, and day columns are also added using dates from \code{dat_in}.  Function is used in \code{\link{wrtds}}.
#'
#' @param grd_in interpolation grid to fill, either a single mean grid or an individual quantile grid created within \code{\link{wrtds}}
#' @param dat_in \code{\link{tidal}} or \code{\link{tidalmean}} object
fill_grd <- function(grd_in, dat_in){

  # fill missing values by column-wise interpolation  
  grd_in <- apply(grd_in, 2, function(x) approx(x, xout = 1:length(x))$y)
  
  # this will fill missing values in last row if present
  grd_in <- t(apply(grd_in, 1, function(x) approx(x, xout = 1:length(x))$y))

  # add year, month, day from dat_in to interp grid
  out <- data.frame(
    date = dat_in$date, 
    year = dat_in$year, 
    month = dat_in$month, 
    day = as.numeric(strftime(dat_in$date, '%d')),
    grd_in
    )

  return(out)
  
}