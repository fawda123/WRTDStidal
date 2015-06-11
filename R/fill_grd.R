#' Fill an interpolation grid using all year, months
#' 
#' Fill an interpolation grid using all year, months within the date range of the original dataset.  This is necessary for predicting data that may not be present in the observed data, as in a user-supplied dataset to the \code{dat_pred} argument in \code{\link{chlpred}}.  Function is used internally in \code{\link{wrtds}}.
#'
#' @param grd_in interpolation grid to fill, either a single mean grid or an individual quantile grid created within \code{\link{wrtds}}
#' @param dat_in \code{\link{tidal}} or \code{\link{tidalmean}} object
fill_grd <- function(grd_in, dat_in){
  
  # add year, month from dat_in to interp grid
  out <- data.frame(year = dat_in$year, month = dat_in$month, grd_in)
  
  # create a master year month grid that includes all year, months
  yrs <- seq(range(out$year)[1], range(out$year)[2])
  mos <- seq(range(out$mo)[1], range(out$mo)[2])
  mast <- expand.grid(month = mos, year = yrs)[, c(2, 1)]
  
  # subset master year month grid by start/end year, month from dat_in
  strt <- out[1, c('year', 'month')] 
  strt <- which(mast$year == strt$year & mast$month == strt$month)
  end <- out[nrow(out), c('year', 'month')]
  end <- which(mast$year == end$year & mast$month == end$month)
  mast <- mast[strt:end, ]
  
  # match the interpolation grid to the master grid, then approximate column-wise
  out <- merge(mast, out, by = c('year', 'month'), all.x = TRUE)
  toapprox <- as.matrix(out[, -c(1, 2)])
  toapprox <- apply(toapprox, 2, function(x) approx(x, xout = 1:length(x))$y)
  out[, -c(1, 2)] <- toapprox
  
  return(out)
  
}