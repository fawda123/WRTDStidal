#' Monthly chlorophyll time series for Hillsborough Bay
#'
#' Monthly chlorophyll time series for the Hillsborough Bay segment of Tampa Bay.  Data are the median values of monthly observations across all water quality stations in Hillsborough Bay.  Date ranges are from January 2000 to December 2012 (156 observations).  Variables are date, chlorophyll-a (in log-space), salinity as fraction of freshwater (i.e., 0 - 1, with higher values indicating more freshwater), and the detection limit for all stations for the respective date.
#'
#' @format A data frame with 156 rows and 4 variables:
#' \describe{
#'   \item{\code{date}}{Date}
#'   \item{\code{chla}}{numeric}
#'   \item{\code{salff}}{numeric}
#'   \item{\code{lim}}{numeric}
#' }
"chldat"