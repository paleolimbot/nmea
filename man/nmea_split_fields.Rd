% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmea-extract.R
\name{nmea_split_fields}
\alias{nmea_split_fields}
\title{Split NMEA into fields}
\usage{
nmea_split_fields(x, names = NULL, split_chars = c(",", "*"))
}
\arguments{
\item{x}{An \code{\link[=nmea]{nmea()}} object.}

\item{names}{Names to assign to fields or \code{NULL} to use a noisily
assigned default.}

\item{split_chars}{A character vector of split characters that delineate
fields.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} with columns \code{names} or fewer if fewer
columns were found in \code{x}.
}
\description{
Split NMEA into fields
}
\examples{
nmea_split_fields(nmea_test_basic)

}
