% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-pkg.R
\name{document_data}
\alias{document_data}
\title{Document Package Data}
\usage{
document_data(path = getwd())
}
\arguments{
\item{path}{Path to the package root that contains the \verb{data-raw/}, \verb{data/}, and \verb{R/} folders. `}
}
\description{
For R packages containing data in the \code{data-raw} dir, the \code{usethis.R} is written in the same directory, the data is documented in the \code{R/data.R}, and \code{devtools::document} is run on the contents of the provided path.
}
