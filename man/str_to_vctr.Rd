% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_to_vctr.R
\name{str_to_vctr}
\alias{str_to_vctr}
\title{A Evaluate a Parsed Expression}
\usage{
str_to_vctr(string)
}
\description{
For the inverse, see \code{\link{vector_to_string}}.
}
\examples{

# String to Vector
test_vctr <- "c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J')"
str_to_vctr(test_vctr)

test_vctr <- 'c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")'
str_to_vctr(test_vctr)

test_vctr <- 'c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\", \"H\", \"I\", \"J\")'
str_to_vctr(test_vctr)


# Vector to String
vctr_to_str(LETTERS[1:5])

vctr_to_str(LETTERS[1:5], quote = "\"")
}
