% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrtdstrnd_sk.R
\name{wrtdstrnd_sk}
\alias{wrtdstrnd_sk}
\alias{wrtdstrnd_sk.default}
\alias{wrtdstrnd_sk.tidal}
\alias{wrtdstrnd_sk.tidalmean}
\title{Get WRTDS trends using seasonal Kendall tests}
\usage{
wrtdstrnd_sk(dat_in, ...)

\method{wrtdstrnd_sk}{default}(dat_in, mobrks, yrbrks, molabs, yrlabs, ...)

\method{wrtdstrnd_sk}{tidal}(
  dat_in,
  mobrks,
  yrbrks,
  molabs,
  yrlabs,
  tau = NULL,
  trndvar = "norm",
  ...
)

\method{wrtdstrnd_sk}{tidalmean}(dat_in, mobrks, yrbrks, molabs, yrlabs, trndvar = "bt_norm", ...)
}
\arguments{
\item{dat_in}{input tidal or tidalmean object which must already have fitted model data}

\item{...}{methods passed to or from other methods}

\item{mobrks}{list of month groupings where each month is an integer from 1 to 12, see examples}

\item{yrbrks}{numeric vector of breaks for years, see examples}

\item{molabs}{character vector of names for month breaks, see examples}

\item{yrlabs}{character vector of names for year breaks, see examples}

\item{tau}{numeric vector of quantile for estimating trends}

\item{trndvar}{chr string of variable for trend evaluation, usually back-transformed, flow-normalized results, see details}
}
\value{
A \code{\link[base]{data.frame}} with summary trends for each grouping, including \code{med} as the median value for the period of observation, \code{tau} as the magnitude and direction of the trend, \code{slope} as the Thiel-Sen slope for change per year, \code{chitest} as the significance test evaluating heterogeneity between seasons, \code{ztest} indicating significance of the overall trend, and \code{perchg} as 100 multiplied by the ratio of the annual slope to the median estimate of the time period (percent change per year).  

As noted in \code{\link{kendallSeasonalTrendTest}}, the overall test is not appropriate if \code{chitest} indicates a small p-value.
}
\description{
Get WRTDS trends using seasonal Kendall tests
}
\details{
Trends are based on \code{\link{kendallSeasonalTrendTest}} for user-specified time periods.  In general, the seasonal Kendall test evaluates monotonic trends using a non-parametric approach that accounts for seasonal variation in the time series.  

All trends are based on back-transformed, flow-normalized results by default. The variable for evaluating trends can be changed with \code{'trndvar'} as \code{'res'}, \code{'norm'}, or \code{'fit'} for \code{tidal} objects and as \code{'res'}, \code{'bt_norm'}, or \code{'bt_fits'} for \code{tidalmean} objects.  In all cases, back-transformed variables are evaluated.

The user must supply the annual and monthly aggregation periods to the appropriate arguments. These are passed to \code{\link[base]{cut}} and are left-open, right-closed along the interval.
}
\examples{
## load a fitted model object
data(tidfit)
data(tidfitmean)

## get trends

# setup month, year categories
mobrks <- list(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(10, 11, 12))
yrbrks <- c(1973, 1985, 1994, 2003, 2012)
molabs <- c('JFM', 'AMJ', 'JAS', 'OND')
yrlabs <- c('1974-1985', '1986-1994', '1995-2003', '2004-2012')

wrtdstrnd_sk(tidfit, mobrks, yrbrks, molabs, yrlabs)
wrtdstrnd_sk(tidfitmean, mobrks, yrbrks, molabs, yrlabs)

}
\references{
Hirsch, R.M., Slack, J.R., Smith, R.A. 1982. Techniques of trend analysis for monthly water quality data. Water Resources Research, 18:107-121.

Millard, S. P. 2013. EnvStats: An R Package for Environmental Statistics. Springer, New York.
}
