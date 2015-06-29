#' Add additional date columns to the interpolation grids.
#' 
#' Add date, year, month, and day columns to the interpolation grids using dates from \code{dat_in}.  Function is used in \code{\link{wrtds}}.
#'
#' @param grd_in interpolation grid to fill, either a single mean grid or an individual quantile grid created within \code{\link{wrtds}}
#' @param dat_in \code{\link{tidal}} or \code{\link{tidalmean}} object
fill_grd <- function(grd_in, dat_in){

  # infinite to NA, this means a really bad fit
  grd_in[is.infinite(grd_in)] <- NA
  
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