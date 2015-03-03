#' Monthly chlorophyll time series for Hillsborough Bay as a tidal object
#'
#' An identical object as \code{\link{tidobj}} with the addition of chlorophyll predictions and normalized estimates after running \code{\link{chlpred}} and \code{\link{chlnorm}}.
#' @format A \code{\link{tidal}} and \code{\link[base]{data.frame}} object with 156 rows and 9 variables:
#' \describe{
#'   \item{\code{date}}{Date}
#'   \item{\code{chla}}{numeric}
#'   \item{\code{salff}}{numeric}
#'   \item{\code{lim}}{numeric}
#'   \item{\code{not_cens}}{logical}
#'   \item{\code{day_num}}{numeric}
#'   \item{\code{month}}{numeric}
#'   \item{\code{year}}{numeric}
#'   \item{\code{dec_time}}{numeric}
#'   \item{\code{fit0.1}}{numeric}
#'   \item{\code{fit0.5}}{numeric}
#'   \item{\code{fit0.9}}{numeric}
#'   \item{\code{norm0.1}}{numeric}
#'   \item{\code{norm0.5}}{numeric}
#'   \item{\code{norm0.9}}{numeric}
#' }
#' 
#' @seealso \code{\link{tidal}} for full list of attributes in tidal objects, \code{\link{wrtds}} for creating the \code{fits} and \code{beta} interpolation grids, and \code{\link{chlpred}} and \code{\link{chlnorm}} for interpolating predicted and normalized values from the grids.
"tidfit"