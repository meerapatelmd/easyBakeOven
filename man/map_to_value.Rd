% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_to_value.R
\name{map_to_value}
\alias{map_to_value}
\title{Map Character Vector to New Values}
\usage{
map_to_value(x, map_assignment, na, other)
}
\arguments{
\item{x}{Character vector.}

\item{map_assignment}{Vector of new values named with \code{x} values. NA values
are viewed as "NA" strings, meaning that NA and "NA" will
be treated as the same value. Map assignment should therefore be \code{c("NA" = "{new_value}")}.}

\item{other}{The value to map all other values not in \code{map_assignment}.  Required only if
\code{map_assignment} is incomplete.}
}
\value{
A character vector of the same length as x.
}
\description{
Map Character Vector to New Values
}
\details{
This function converts \code{x} to factor, recodes the factor, and then
returns the recoded factor vector as character.
}
\examples{
\dontrun{
x <- c('T', 'R', 'H', 'W', 'P', 'NA', NA_character)
map_to_value(
  x = x,
  map_assignment =
  c('T' = 'red',
    'R' = 'green'),
  other = 'black'
)

map_to_value(
  x = x,
  map_assignment =
  c('T' = 'red',
    'R' = 'green',
    'NA' = 'white'),
  other = 'black'
)
}
}
\concept{mapping functions}
