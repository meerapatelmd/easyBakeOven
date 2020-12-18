#' @title
#' Make Pkgdown YAML Template
#' @description
#' Prints a pkgdown yaml template to the R console for copy-and-pasting. To get true references to copy-and-paste into the pkgdown yaml, see \code{\link{make_pkgdown_yaml_refs}}.
#' @rdname make_pkgdown_yaml
#' @export
make_pkgdown_yaml <-
  function() {
    cat(readLines(system.file("templates",
                              "rlang_pkgdown.yml",
                              package = "easyBakeOven")),
        sep = "\n")
  }

#' @title
#' Make a List of References for Pkgdown YAML
#' @description
#' Prints yaml format references for copy-and-pasting into the pkgdown yaml file. To print a template, see \code{\link{make_pkgdown_yaml}}.
#' @seealso
#'  \code{\link[stringr]{str_remove}}
#' @rdname make_pkgdown_yaml_refs
#' @export
#' @importFrom stringr str_remove_all
make_pkgdown_yaml_refs <-
  function() {

    reference_path <- file.path(getwd(),
                                "docs",
                                "reference")

    html_files <-
      list.files(
        path = reference_path,
        pattern = "[.]{1}html$"
      )

    reference_names <-
      stringr::str_remove_all(
        string = html_files,
        pattern = "[.]{1}html"
      )

    sprintf("- %s", reference_names) %>%
      cat(sep = "\n")
  }
