#' Linearly interpolate non-unique numbers
#' 
#' Linearly interpolate non-unique numbers, a modification to the standard \code{\link[stats]{approx}} function.
#'
#' @param x numeric of x locations bounding the interpolation
#' @param y numeric of y locations bounding the interpolation
#' @param xint numeric x location to interpolate y
#' 
#' @details Used in \code{\link{chlinterp}}
#' 
#' @return The \code{y} value if all values are non-unique, otherwise an interpolated value defined by \code{xint}
approx_uni <- function(x, y, xint){
  
  # if y is the same at both x endpoints, return y
  chk_y <- suppressWarnings(unique(as.numeric(y)))
  if(length(chk_y) == 1) return(chk_y)
  
  # otherwise approx
  out <- try({approx(x, y, xint)$y})
  if(class(out) %in% 'try-error') browser()
  return(out)
  
}