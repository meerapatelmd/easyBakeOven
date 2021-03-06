#' @title
#' Read DESCRIPTION as a Dataframe
#'
#' @description
#' Read the DESCRIPTION file as a 2-column dataframe for the header name along with the value/s following the colon of that header. To make these values in the R console, see \code{\link{makeDescription}}.
#'
#' @examples
#' read_description()
#' @family DESCRIPTION functions
#' @family R Package functions
#' @seealso
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[tidyr]{unite}}
#' @rdname read_description
#' @export
#' @importFrom stringr str_replace_all
#' @importFrom tibble tibble
#' @importFrom tidyr unite



read_description <-
  function(path = getwd()) {
    path <- path.expand(file.path(path, "DESCRIPTION"))


    DESCRIPTION <- readLines(con = path)
    starting_indexes <- grep(
      pattern = "[:]{1}",
      x = DESCRIPTION
    )
    ending_indexes <- starting_indexes[-1]
    ending_indexes <- ending_indexes - 1
    ending_indexes <- c(ending_indexes, length(DESCRIPTION))


    for (i in seq_along(starting_indexes)) {
      if (ending_indexes[i] < starting_indexes[i]) {
        stop(sprintf(
          "%s: ending index is %s, lesser than starting index of %s",
          i,
          ending_indexes[i],
          starting_indexes[i]
        ))
      }
    }

    starting_line <- DESCRIPTION[starting_indexes]
    headers <-
      stringr::str_replace_all(
        string = starting_line,
        pattern = "(^.*?):(.*$)",
        replacement = "\\1"
      )

    values0 <-
      stringr::str_replace_all(
        string = starting_line,
        pattern = "(^.*?):(.*$)",
        replacement = "\\2"
      )
    values0 <- trimws(values0)
    values0[values0 %in% c("")] <- NA_character_

    values <- list()
    for (i in seq_along(starting_indexes)) {
      values[[i]] <- DESCRIPTION[(starting_indexes[i]):ending_indexes[i]]
      values[[i]] <- grep(
        pattern = "[:]{1}",
        x = values[[i]],
        invert = TRUE,
        value = TRUE
      )
      values[[i]] <- trimws(values[[i]], which = "both")
      values[[i]] <- paste(values[[i]], collapse = "\n")
    }
    values <- unlist(values)
    values[values %in% c("")] <- NA_character_


    tibble::tibble(
      headers = headers,
      values0 = values0,
      values = values
    ) %>%
      tidyr::unite(
        col = value,
        values0,
        values,
        sep = " ",
        remove = TRUE,
        na.rm = TRUE
      )
  }



#' @title
#' Read NAMESPACE as a List
#'
#' @description
#' Read the NAMESPACE file as a list split by whether it is tagged as an export, import, or an importFrom. The export and imports are returned as a list of package names while the importFrom is a dataframe of the package name and the function from the package.
#'
#' @examples
#' read_namespace()
#' @family NAMESPACE functions
#' @family R Package functions

read_namespace <-
  function(path = getwd()) {
    path <- path.expand(file.path(path, "NAMESPACE"))

    NAMESPACE <- readLines(con = path)
    NAMESPACE <- grep(
      pattern = "# Generated by roxygen2: do not edit by hand",
      x = NAMESPACE,
      fixed = TRUE,
      ignore.case = FALSE,
      invert = TRUE,
      value = TRUE
    )

    NAMESPACE <- NAMESPACE[!(NAMESPACE %in% c(""))]

    output <-
      tibble::tibble(NAMESPACE = NAMESPACE) %>%
      tidyr::extract(
        col = NAMESPACE,
        into = c("direction", "argument"),
        regex = "(^.*?)\\((.*?)\\)"
      ) %>%
      dplyr::mutate(argument2 = ifelse(!(direction %in% c("export")),
        argument,
        NA_character_
      )) %>%
      tidyr::separate(
        col = argument2,
        into = c("argument2a", "argument2b"),
        sep = "[,]{1}",
        remove = FALSE
      ) %>%
      rubix::split_deselect(direction)

    output$export <-
      output$export %>%
      dplyr::select(export_fun = argument) %>%
      unlist()


    output$importFrom <-
      output$importFrom %>%
      dplyr::select(
        pkg = argument2a,
        pkg_fun = argument2b
      )


    if ("import" %in% names(output)) {
      output$import <-
        output$import %>%
        dplyr::select(import_pkg = argument2) %>%
        unlist()
    }


    output
  }
