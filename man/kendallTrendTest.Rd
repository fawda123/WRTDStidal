% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kendallTrendTest.R
\name{kendallTrendTest}
\alias{kendallTrendTest}
\alias{kendallTrendTest.default}
\alias{kendallTrendTest.formula}
\title{Kendall trend test}
\usage{
kendallTrendTest(y, ...)

\method{kendallTrendTest}{default}(
  y,
  x = seq(along = y),
  alternative = "two.sided",
  correct = TRUE,
  ci.slope = TRUE,
  conf.level = 0.95,
  warn = TRUE,
  data.name = NULL,
  data.name.x = NULL,
  parent.of.data = NULL,
  subset.expression = NULL,
  ...
)

\method{kendallTrendTest}{formula}(y, data = NULL, subset, na.action = na.pass, ...)
}
\arguments{
\item{y}{an object containing data for the trend test. In the default method, the argument \code{y} must be numeric vector of observations. In the formula method, \code{y} must be a formula of the form \code{y ~ 1} or \code{y ~ x}. The form \code{y ~ 1} indicates use the observations in the vector \code{y} for the test for trend and use the default value of the argument \code{x} in the call to \code{kendallTrendTest.default}. The form \code{y ~ x} indicates use the observations in the vector \code{y} for the test for trend and use the specified value of the argument \code{x} in the call to \code{kendallTrendTest.default}. Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf, -Inf}) values are allowed but will be removed.}

\item{...}{methods passed to or from other methods}

\item{x}{numeric vector of "predictor" values. The length of \code{x} must equal the length of \code{y}. Missing (\code{NA}), undefined (\code{NaN}), and infinite (\code{Inf, -Inf}) values are allowed but will be removed. The default value of \code{x} is the vector of numbers \code{1, 2, ..., n} where \code{n} is the number of elements in \code{y}.}

\item{alternative}{character string indicating the kind of alternative hypothesis. The possible values are \code{"two.sided"} (tau not equal to 0; the default), \code{"less"} (tau less than 0), and \code{"greater"} (tau greater than 0).}

\item{correct}{logical scalar indicating whether to use the correction for continuity in computing the z-statistic that is based on the test statistic S'. The default value is \code{TRUE}.}

\item{ci.slope}{logical scalar indicating whether to compute a confidence interval for the slope. The default value is \code{TRUE}.}

\item{conf.level}{numeric scalar between 0 and 1 indicating the confidence level associated with the confidence interval for the slope. The default value is \code{0.95}.}

\item{warn}{logical scalar indicating whether to print a warning message when \code{y} does not contain at least two non-missing values, or when \code{x} does not contain at least two unique non-missing values. The default value is \code{TRUE}.}

\item{data.name}{character string indicating the name of the data used for the trend test. The default value is \code{deparse(substitute(y))}.}

\item{data.name.x}{character string indicating the name of the data used for the predictor variable \code{x}. If \code{x} is not supplied this argument is ignored. When \code{x} is supplied, the default value is \code{deparse(substitute(x))}.}

\item{parent.of.data}{character string indicating the source of the data used for the trend test.}

\item{subset.expression}{character string indicating the expression used to subset the data.}

\item{data}{specifies an optional data frame, list or environment (or object coercible by \code{as.data.frame} to a data frame) containing the variables in the model. If not found in \code{data}, the variables are taken from \code{environment(formula)}, typically the environment from which \code{kendallTrendTest} is called.}

\item{subset}{specifies an optional vector specifying a subset of observations to be used.}

\item{na.action}{specifies a function which indicates what should happen when the data contain \code{NAs}. The default is \code{\link[stats]{na.pass}}.}
}
\value{
A list object with elements for results of the test
}
\description{
Nonparametric test for monotonic trend based on Kendall's Tau statistic
}
\details{
\code{kendallTrendTest} performs Kendall's nonparametric test for a monotonic trend, which is a special case of the test for independence based on Kendall's tau statistic (see \code{\link[stats]{cor.test}}). The slope is estimated using the method of Theil (1950) and Sen (1968). When \code{ci.slope=TRUE}, the confidence interval for the slope is computed using Gilbert's (1987) Modification of the Theil/Sen Method.

Kendall's test for a monotonic trend is a special case of the test for independence based on Kendall's tau statistic. The first section below explains the general case of testing for independence. The second section explains the special case of testing for monotonic trend. The last section explains how a simple linear regression model is a special case of a monotonic trend and how the slope may be estimated.
}
\examples{
kendallTrendTest(res ~ dec_time, tidfitmean)
}
\references{
Hirsch, R.M., Slack, J.R., Smith, R.A. 1982. Techniques of trend analysis for monthly water quality data. Water Resources Research, 18:107-121.
 
Millard, S. P. 2013. EnvStats: An R Package for Environmental Statistics. Springer, New York.
}
