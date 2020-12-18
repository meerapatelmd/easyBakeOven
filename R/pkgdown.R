#' @export

copyPkgdownTemplate <-
  function() {
    file.copy(
      from =
        system.file("templates",
          "rlang_pkgdown.yml",
          package = "easyBakeOven"
        ),
      to =
        "template_pkgdown.yml",
      overwrite = TRUE
    )
  }


#' @export

makeReferenceSection <-
  function() {
    '
                reference:
                        - title: Tidy evaluation
                        desc: >
                                Quote arguments and expressions with unquoting support. The
                        quosure variants wrap an environment with the expression.
                        contents:
                                - quo
                                - quos
                                - enquo
                                - starts_with("arrange)
                                - matches("test")
                '
  }


#' @export

makeReferenceList <-
  function(reference_path) {
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
