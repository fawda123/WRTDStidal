% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/respred.R
\name{respred}
\alias{respred}
\alias{respred.tidal}
\alias{respred.tidalmean}
\title{Get WRTDS predictions from interpolation grids}
\usage{
respred(dat_in, ...)

\method{respred}{tidal}(dat_in, dat_pred = NULL, trace = TRUE, omit = TRUE, ...)

\method{respred}{tidalmean}(dat_in, dat_pred = NULL, trace = TRUE, omit = TRUE, ...)
}
\arguments{
\item{dat_in}{input tidal or tidalmean object}

\item{...}{arguments passed to or from other methods}

\item{dat_pred}{optional data to predict using the interpolation grids in dat_in, defaults to observed data in \code{dat_in} if not supplied, see details}

\item{trace}{logical indicating if progress is shown in the console}

\item{omit}{logical indicating if observations in \code{dat_pred} that are outside of the range of data used to fit the model are removed, see details}
}
\value{
Appends columns to the input data.frame for the predicted values.  For tidal objects, columns are named starting with the prefix `fit', e.g., `fit0.5' are the predicted values for the fit through the median.  For tidalmean objects, predicted values are appended for the mean model in log-space and the observed values from the back-transformed grids.  Columns are named as `fits' and `bt_fits'.
}
\description{
Get model predictions from WRTDS using linear interpolation of values in grids
}
\details{
This function is used after \code{wrtds} to estimate predicted values of the response variable from the interpolation grids.  The estimated values are based on a bilinear interpolation of the four predicted response values at two salinity/flow and two date values nearest to the observed salinity/flow and date values to predict.  

Data for \code{dat_pred} must be a data frame of two columns for date and flow variables (\code{date} and \code{numeric} objects).  The columns must be named 'date' and 'flo'.  Values that are outside of the range of data used to fit the model are removed with a warning.  It is assumed that the flow variable is not scaled (i.e., raw data) as in a \code{tidal} or \code{tidalmean} object. The dimensions of the output data are modified to match \code{dat_pred} if observations are removed.  The \code{omit} argument should not equal \code{FALSE} and is included only for use with \code{\link{wrtdscv}} to evaluate folds of the original dataset.
}
\examples{
##

# load a tidal object
data(tidobj)

# get fitted values for each quantile
res <- respred(tidobj)

# load a tidalmean object
data(tidobjmean)

# get predicted values
res <- respred(tidobjmean)

}
