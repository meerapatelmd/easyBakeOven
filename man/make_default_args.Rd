% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make.R
\name{make_default_args}
\alias{make_default_args}
\title{Make a Formal Argument Skeleton with Default Values}
\usage{
make_default_args(fun)
}
\arguments{
\item{fun}{Function object.}
}
\description{
Retrieve the formal arguments with the default values to copy-and-paste as formal arguments of another function. To copy-and-paste formal arguments as part of an internal function, see \code{\link{makeInternalArgs}}.
}
\examples{
make_default_args(read.csv)
make_internal_args(read.csv)
make_args(read.csv)
make_arg_declaration(read.csv)
}
