#' @title
#' Map Vector to Colors
#'
#' @description
#' Will abort if `color_assignment` does not contain valid colors in `colors_hex_map()` or `hex_colors_map()`.
#'
#' @param x Character vector.
#' @param color_assignment Vector of new values named with `x` values. NA values
#' are viewed as "NA" strings, meaning that NA and "NA" will
#' be treated as the same value. Map assignment should therefore be 'c("NA" = "{new_value}")'.
#'
#' @param other The value to map all other values not in `color_assignment`. Required only if
#' `color_assignment` is incomplete.
#'
#' @return
#' A character vector of the same length as x.
#'
#' @details
#' This function converts `x` to factor, recodes the factor, and then
#' returns the recoded factor vector as character using `map_to_value()`.
#'
#' @details DETAILS
#' @rdname map_to_color
#' @export
#' @importFrom stringr str_replace_na
map_to_color <-
        function(x,
                 color_assignment,
                 other) {

                colors <-
                unique(
                c(color_assignment,
                  other)
                )

                if (!all(colors %in% colors_hex_map()) & !all(colors %in% hex_colors_map())) {
                        cli::cli_abort(
                                "All colors found in {.var color_assignment} and {.var other} are not
                                recognized in {.fn colors_hex_map} or {.fn hex_colors_map}."
                        )
                }

                map_to_value(
                        x = x,
                        map_assignment = color_assignment,
                        other = other
                )

        }



#' @title
#' Map Vector to Colors
#'
#' @description
#' Colors can be listed using `colors_hex_map()`.
#'
#' @param x PARAM_DESCRIPTION
#' @param color_assignment PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @rdname map_colors
#' @export
#' @importFrom stringr str_replace_na
map_colors <-
        function(x,
                 color_assignment,
                 other) {

                .Deprecated(new = "map_to_color")

                map_to_value(
                        x = x,
                        map_assignment = color_assignment,
                        other = other
                )

                # color_assignment2 <- names(color_assignment)
                # names(color_assignment2) <-
                #         stringr::str_replace_na(color_assignment)
                #
                #
                #
                # fct_recode(factor(stringr::str_replace_na(x)),
                #            !!!color_assignment2) %>%
                #         as.character()

        }
