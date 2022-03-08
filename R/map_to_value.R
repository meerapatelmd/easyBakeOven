#' @title
#' Map Character Vector to New Values
#'
#' @param x Character vector.
#' @param map_assignment Vector of new values named with `x` values. \emph{NA} values
#' are viewed as \emph{"NA"} strings, meaning that \emph{NA} and \emph{"NA"} will
#' be treated as the same value. Map assignment should therefore be \code{c("NA" = "{new_value}")}}.
#'
#' @param other The value to map all other values not in `map_assignment`.  Required only if
#' `map_assignment` is incomplete.
#'
#' @return
#' A character vector of the same length as x.
#'
#' @details
#' This function converts `x` to factor, recodes the factor, and then
#' returns the recoded factor vector as character.
#'
#' @rdname map_to_value
#' @family mapping functions
#' @export
#' @importFrom stringr str_replace_na
#' @importFrom cli cli_alert_warning cli_alert_success
#' @importFrom forcats fct_recode
#' @examples
#' x <- c('T', 'R', 'H', 'W', 'P', 'NA', NA_character)
#' map_to_value(
#'   x = x,
#'   map_assignment =
#'   c('T' = 'red',
#'     'R' = 'green'),
#'   other = 'black'
#' )
#'
#' map_to_value(
#'   x = x,
#'   map_assignment =
#'   c('T' = 'red',
#'     'R' = 'green',
#'     'NA' = 'white'),
#'   other = 'black'
#' )


map_to_value <-
function (x,
          map_assignment,
          na,
          other) {
        stopifnot(is.character(x))
        x <- stringr::str_replace_na(x, replacement = "NA")
        levels <- names(map_assignment)
        names(levels) <- map_assignment
        unique_x <- unique(x)
        missing_levels <- unique_x[!(unique_x %in% levels)]
        if (length(missing_levels) > 0) {
                cli::cli_alert_warning("{length(missing_levels)} value{?s} not in {.var map_assignment}: {.emph {missing_levels}}. Mapping to {.var other} value {.emph {other}}.")
                names(missing_levels) <- rep(other, length(missing_levels))
                levels <- c(levels, missing_levels)
        }
        else {
                cli::cli_alert_success("All {length(unique_x)} unique value{?s} mapped.")
        }
        suppressWarnings(forcats::fct_recode(factor(x), !!!levels) %>%
                                 as.character())
}
