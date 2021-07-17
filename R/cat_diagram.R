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
                        # Split befre dash or less than sign
                        strsplit(flow,
                                 split = "(?<=[ ]{1})(?=[-<]{1}[^ ]{1,})",
                                 perl = TRUE) %>%
                        unlist() %>%
                        strsplit(split = "(?<=[>-]{1})(?=[ ]{1})",
                                 perl = TRUE) %>%
                        unlist() %>%
                        trimws(which = "both") %>%
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


#' @title
#' Concatenate a Customized Diagram
#' @description
#' More custom features passed onto `cli::cat_boxx`
#' by each node in a diagram.
#'
#' @inheritParams cat_diagram
#' @param ... A list named by the string representing a node in the
#' `flow` string that contains a list of argument-value pair that will
#' be passed to the `cli::cat_boxx` function.
#'
#' @details
#' Example of a named list of customizations passed to the function:
#' ```
#' flow <-
#'   "Test Custom Box -> Test Custom Box 2 -> Test Custom Box 3"
#'
#'  cat_custom_diagram(
#'    flow = flow,
#'    'Test Custom Box 2' =
#'       list(header = "Database"),
#'    'Test Custom Box 3' =
#'       list(border_style = "single-double"))
#' ```
#'
#' @seealso
#'  \code{\link[cli]{console_width}},\code{\link[cli]{cat_line}}
#'  \code{\link[rlang]{list2}},\code{\link[rlang]{parse_expr}}
#'  \code{\link[purrr]{transpose}},\code{\link[purrr]{map}}
#'  \code{\link[utils]{capture.output}}
#'  \code{\link[glue]{glue}}
#' @rdname cat_custom_diagram
#' @export
#' @importFrom cli console_width cat_boxx
#' @importFrom rlang list2 parse_expr
#' @importFrom purrr transpose map
#' @importFrom utils capture.output
#' @importFrom glue glue



cat_custom_diagram <-
        function(flow,
                 ...,
                 border_style = "single",
                 padding = 1,
                 margin = 0,
                 float = c("left", "center", "right"),
                 col = NULL,
                 background_col = NULL,
                 border_col = col,
                 align = c("left", "center", "right"),
                 width = cli::console_width()) {


                custom_diagram_params <-
                        rlang::list2(...)

                if (length(custom_diagram_params)>0) {

                        max_params <-
                                custom_diagram_params %>%
                                purrr::transpose()

                        if ("padding" %in% names(max_params)) {
                                padding <-
                                max(c(padding,unlist(max_params$padding)),
                                    na.rm = TRUE)

                        }

                        if ("margin" %in% names(max_params)) {
                                margin <-
                                        max(c(margin,unlist(max_params$margin)),
                                            na.rm = TRUE)

                        }




                }


                inp <-
                        # Split befre dash or less than sign
                        strsplit(flow,
                                 split = "(?<=[ ]{1})(?=[-<]{1}[^ ]{1,})",
                                 perl = TRUE) %>%
                        unlist() %>%
                        strsplit(split = "(?<=[>-]{1})(?=[ ]{1})",
                                 perl = TRUE) %>%
                        unlist() %>%
                        trimws(which = "both") %>%
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

                                if (!(x %in% names(custom_diagram_params))) {

                                out[[i]] <-
                                        utils::capture.output(cli::cat_boxx(x #,
                                                                            # border_style = border_style,
                                                                            # padding = padding,
                                                                            # margin = margin,
                                                                            # float = float,
                                                                            # col = col,
                                                                            # background_col = background_col,
                                                                            # border_col = border_col,
                                                                            # align = align,
                                                                            # width = width
                                                                            )) %>%
                                        as.list()

                                } else {

                                        custom_params_list <-
                                                custom_diagram_params[[x]]

                                        label <- x
                                        nms   <- names(custom_params_list)

                                        custom_arg_expr <-
                                                vector(mode = "list",
                                                       length = length(custom_params_list))
                                        for (j in seq_along(custom_params_list)) {
                                                nm <- names(custom_params_list)[j]

                                                value <- custom_params_list[[j]]

                                                if (is.character(value)) {
                                                        if (!is.null(value)) {
                                                                value <- vctr_to_str(value)
                                                        }

                                                }

                                                if (is.null(value)) {
                                                        value <- "NULL"
                                                }


                                                custom_arg_expr[j] <-
                                                        glue::glue("  {nm} = {value}")

                                        }
                                        argument_expr <-
                                                paste(custom_arg_expr,
                                                      collapse = ",")

                                        expr <-
                                                glue::glue(
                                                        "utils::capture.output(",
                                                        "cli::cat_boxx(",
                                                        "  label = '{label}',",
                                                        "{argument_expr}",
                                                        ")) %>% as.list()"
                                                )


                                        out[[i]] <-
                                                eval(rlang::parse_expr(expr))








                                }

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
