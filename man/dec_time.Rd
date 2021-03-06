% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dec_time.R
\name{dec_time}
\alias{dec_time}
\alias{dec_time.Date}
\title{Create decimal time from time vector}
\usage{
dec_time(date_in)

\method{dec_time}{Date}(date_in)
}
\arguments{
\item{date_in}{input time vector, usually a \code{date} object}
}
\value{
A named list of four numeric vectors including \code{day_num} (decimal day on an annual scale), \code{month} (month of the year as integer), \code{year}, and \code{dec_time} (decimal time as sum of \code{year} and \code{day_num})
}
\description{
Create decimal time on an annual scale from an input time vector
}
\details{
Function is used internally within the package.
}
\examples{
dt <- Sys.Date()
dts <- seq.Date(dt - 365, dt, by = 'day') 

dec_time(dts)
}
