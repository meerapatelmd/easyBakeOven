% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report.R
\name{create_report_child}
\alias{create_report_child}
\title{Create a Child Rmd from a Template's Child}
\usage{
create_report_child(
  template_path,
  child_rmd_folder,
  require_all_vars = TRUE,
  edit_file = TRUE,
  ...
)
}
\arguments{
\item{template_path}{Path to the template file to use. It can point to a
template Rmd in your local machine.}

\item{child_rmd_folder}{Path that the template will be copied to.}

\item{require_all_vars}{Variables to replace are designated as those enclosed
in curly brackets in the template Rmd file. Sometimes, the template may have
variables to be called upon by the glue function iteratively outside the
template itself. This argument controls whether or not all of these variables
must be assigned a value or not. Default: TRUE.}

\item{edit_file}{If called in an interactive session, the file will be opened in R Studio if TRUE.}

\item{...}{(Optional) Additional parameters that should be added in the
front matter of the Rmd. Should be entered in the format of a named vector.}
}
\description{
Sometimes a template Rmd will also have children.
This function makes a copy of the template child at the given \code{template_path}
and copies it to the \code{child_rmd_folder} with the same filename. Unlike
\code{create_report()}, subdirectories are not created since that is the
work of the main template.
}
\seealso{
\code{\link[rlang]{list2}}
\code{\link[xfun]{read_utf8}}
\code{\link[stringr]{str_replace}}
\code{\link[cli]{cli_alert}}
}
