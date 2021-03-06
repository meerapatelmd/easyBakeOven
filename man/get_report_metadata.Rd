% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{get_report_metadata}
\alias{get_report_metadata}
\title{Return a Nested List of Report Metadata}
\usage{
get_report_metadata(
  issue_key,
  report_title,
  project_path = getwd(),
  github_page_path = "",
  source_code_page_path = "",
  template_path = system.file(package = "easyBakeOven", "reports", "Generic.Rmd"),
  ...
)
}
\arguments{
\item{issue_key}{Issue identifier.}

\item{report_title}{Name of the new report. It cannot contain any
forward slashes.}

\item{project_path}{Project path assigned to the new Rmd file.}

\item{github_page_path}{Path to the GitHub site that will host the html
output if the feature is enabled.}

\item{source_code_page_path}{Path to the GitHub repository source code
for the Rmd file.}

\item{template_path}{Path to the template file to use. It can point to a
template Rmd in your local machine.}

\item{...}{(Optional) Additional parameters that should be added in the
front matter of the Rmd. Should be entered in the format of a named vector.}
}
\description{
Return a Nested List of Report Metadata
}
\seealso{
\code{\link[rlang]{list2}}
\code{\link[tibble]{tibble}}
\code{\link[xfun]{file_ext}}
}
