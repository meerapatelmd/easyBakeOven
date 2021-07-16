#' @title
#' Concatenate a Diagram
#' @description
#' Concatenate a diagram in a left to right direction.
#' @param flow String that alternates between a box label and arrows beginning
#' with either `-` or `<`.
#' @inheritParams cli::cli_boxx
#' @seealso
#'  \code{\link[cli]{console_width}},\code{\link[cli]{cat_line}}
#' @rdname cat_diagram
#' @export
#' @importFrom cli console_width cat_boxx
#' @importFrom purrr transpose map %>%

cat_diagram <-
        function(flow,
                 border_style = "single",
                 padding = 1,
                 margin = 0,
                 float = c("left", "center", "right"),
                 col = NULL,
                 background_col = NULL,
                 border_col = col,
                 align = c("left", "center", "right"),
                 width = cli::console_width()) {

                inp <-
                        strsplit(flow,
                                 split = " ") %>%
                        unlist() %>%
                        as.list()

                out <- list()
                for (i in seq_along(inp)) {

                        x <- inp[[i]]
                        if (grepl(pattern = "^[-<]{1}", x)) {

                                out[[i]] <-
                                        utils::capture.output(cli::cat_boxx(x,
                                                                     border_style = "none",
                                                                     padding = padding,
                                                                     margin = margin,
                                                                     float = float,
                                                                     col = col,
                                                                     background_col = background_col,
                                                                     border_col = border_col,
                                                                     align = align,
                                                                     width = width)) %>%
                                        as.list()

                        } else {

                                out[[i]] <-
                                        utils::capture.output(cli::cat_boxx(x,
                                                                     border_style = border_style,
                                                                     padding = padding,
                                                                     margin = margin,
                                                                     float = float,
                                                                     col = col,
                                                                     background_col = background_col,
                                                                     border_col = border_col,
                                                                     align = align,
                                                                     width = width)) %>%
                                        as.list()

                        }

                        names(out[[i]]) <- sprintf("Line%s", 1:length(out[[i]]))
                        names(out)[i] <- x

                }


                out %>%
                        purrr::transpose() %>%
                        purrr::map(unlist) %>%
                        purrr::map(paste, collapse = "") %>%
                        unlist() %>%
                        cat(sep = "\n")

        }
