% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify_roxygen2.R
\name{add_deprecated_call}
\alias{add_deprecated_call}
\title{Add \code{.Deprecated()} to Text}
\usage{
add_deprecated_call(text, new = NULL)
}
\arguments{
\item{text}{Character vector containing function declarations.}

\item{new}{(Optional) Argument passed to the new call to \code{.Deprecated()}.}
}
\description{
\code{.Deprecated()} is added after each function declaration in the text. Note that it does not check for an existing \code{.Deprecated()}.
}
\examples{
library(tidyverse)

# Adds after each function declaration
add_deprecated_call("sample_function1 <-\nfunction(a,b,c){}\n\n#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}")

# Does not skip function declarations that already makes a call to the .Deprecated function
add_deprecated_call("sample_function1 <-\nfunction(a,b,c){\n.Deprecated()\n}\n\n#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}")

# Text can be a vector of length greater than 1
add_deprecated_call(c("sample_function1 <-\nfunction(a,b,c){\n.Deprecated()\n}", "#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}"))
}
\seealso{
\code{\link[stringr]{str_replace}}

Other modify roxygen2 documentation functions: 
\code{\link{add_deprecated_to_desc}()}
}
\concept{modify roxygen2 documentation functions}
