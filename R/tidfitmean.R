#' Monthly chlorophyll time series for Hillsborough Bay as a tidal object for the conditional mean model
#'
#' An identical object as \code{\link{tidobjmean}} with the addition of predicted and normalized chlorophyll (log-space and back-transformed) after running \code{\link{chlpred}} and \code{\link{chlnorm}}.
#' @format A \code{\link{tidalmean}} and \code{\link[base]{data.frame}} object with 156 rows and 11 variables:
#' \describe{
#'   \item{\code{date}}{Date}
#'   \item{\code{chla}}{numeric}
#'   \item{\code{sal}}{numeric}
#'   \item{\code{lim}}{numeric}
#'   \item{\code{not_cens}}{logical}
#'   \item{\code{day_num}}{numeric}
#'   \item{\code{month}}{numeric}
#'   \item{\code{year}}{numeric}
#'   \item{\code{dec_time}}{numeric}
#'   \item{\code{fits}}{numeric}
#'   \item{\code{bt_fits}}{numeric}
#'   \item{\code{norm}}{numeric}
#'   \item{\code{bt_norms}}{numeric}
#' }
#' 
#' @seealso \code{\link{tidalmean}} for full list of attributes in tidalmean objects, \code{\link{wrtds}} for creating the \code{fits} and \code{bt_fits} interpolation grids in the attributes, and \code{\link{chlpred}} and \code{\link{chlnorm}} for interpolating predicted and normalized values from the grids.
"tidfitmean"