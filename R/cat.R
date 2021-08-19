#' @title
#' Make a List Object
#' @rdname cat_list
#' @export
#' @importFrom glue glue



cat_list <-
        function(list) {
                nms <- names(list)

                output <-
                        vector(mode = "character",
                               length = length(list))
                for (i in seq_along(list)) {
                        nm <- names(list)[i]

                        value <- list[[i]]
                        value <- vctr_to_str(value)


                        output[i] <-
                        glue::glue("  `{nm}` = {value}")

                }


                cat(
                "list(",
                paste(output,
                      collapse = ",\n"),
                ")",
                sep = "\n"
                )

        }
