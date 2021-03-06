% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resscls.R
\name{resscls}
\alias{resscls}
\alias{resscls.tidalmean}
\title{Get the scale parameters for predicted values}
\usage{
resscls(dat_in, ...)

\method{resscls}{tidalmean}(dat_in, dat_pred = NULL, ...)
}
\arguments{
\item{dat_in}{input tidalmean object}

\item{...}{arguments passed to or from other methods}

\item{dat_pred}{optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied}
}
\value{
Appends columns to the data.frame for the associated scale value for the predicted values.  A column is appended to the \code{dat_in} object, named `scls'.
}
\description{
Get the scale parameters for predicted values of the response variable, only applies to \code{\link{tidalmean}} objects.
}
\details{
This function is used after \code{wrtds} to get scale parameters for predicted values of the response variable from the interpolation grids.  The values are based on a bilinear interpolation of the four predicted response values at two salinity/flow and two date values nearest to the observed salinity/flow and date values to predict.
}
\examples{
##

# load a tidalmean object
data(tidobjmean)

# get predicted values
res <- resscls(tidobjmean)

}
