#' @title
#' Add `.Deprecated()` to Text
#' @description
#' `.Deprecated()` is added after each function declaration in the text. Note that it does not check for an existing `.Deprecated()`.
#'
#' @example inst/example/add_deprecated_call.R
#' @param text Character vector containing function declarations.
#' @param new  (Optional) Argument passed to the new call to `.Deprecated()`.
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#' @rdname add_deprecated_call
#' @family modify roxygen2 documentation functions
#' @export
#' @importFrom stringr str_replace_all



add_deprecated_call <-
  function(text,
           new = NULL) {

    if (!any(grepl(pattern = ".Deprecated()",
               x = text,
               ignore.case = FALSE,
               fixed = TRUE))) {
    if (is.null(new)) {
      stringr::str_replace_all(
        string = text,
        pattern = "(function[(]{1}.*?[)]{1}[{]{1})",
        replacement = "\\1\n\t\t\t.Deprecated()\n"
      )
    } else {
      stringr::str_replace_all(
        string = text,
        pattern = "(function[(]{1}.*?[)]{1}[{]{1})",
        replacement = sprintf(
          "\\1\n\t\t\t.Deprecated(new = '%s')\n",
          new
        )
      )
    }
    } else {
      cli::cli_alert_info("`.Deprecated()` already in text")
      return(text)
    }
  }


#' @title
#' Add "(Deprecated)" after the `@description` Roxygen2 Tag
#' @param text Character vector containing function declarations.
#' @example inst/example/add_deprecated_to_desc.R
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#' @rdname add_deprecated_to_desc
#' @family modify roxygen2 documentation functions
#' @export
#' @importFrom stringr str_replace_all


add_deprecated_to_desc <-
  function(text) {

    if (!any(grepl(pattern = "@description (Deprecated)",
                   x = text,
                   fixed = TRUE))) {
        stringr::str_replace_all(
          string = text,
          pattern = "(@description)",
          replacement = "\\1 (Deprecated)"
        )
    } else {
      cli::cli_alert_info("`@description (Deprecated)` already in text")
      return(text)
    }
  }
