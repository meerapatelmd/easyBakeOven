#' @title
#' `DT::datatable()` Wrapper
#'
#' @inheritParams DT::datatable
#' @param download_buttons Include download buttons at the top of the table?, Default: TRUE
#' @param scrollX Fix the width of the width of the table?, Default: TRUE
#' @seealso
#'  \code{\link[DT]{datatable}}
#' @rdname print_dt
#' @export
#' @importFrom DT datatable

print_dt <-
        function(data,
                 escape = FALSE,
                 download_buttons = TRUE,
                 caption = NULL,
                 rownames = FALSE,
                 pageLength = 10,
                 autoWidth = TRUE,
                 colnames,
                 filter = c("top", "none", "bottom"),
                 scrollX = TRUE,
                 style = "auto",
                 class = "display",
                 container,
                 width = NULL,
                 height = NULL,
                 elementId = NULL,
                 fillContainer = getOption("DT.fillContainer", NULL),
                 autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
                 selection = c("multiple", "single", "none"),
                 plugins = NULL,
                 editable = FALSE) {

                filter <-
                        match.arg(arg = filter,
                                  choices = c("top", "none", "bottom"),
                                  several.ok = FALSE)


                if (download_buttons) {
                DT::datatable(
                        data = data,
                        caption = caption,
                        rownames = rownames,
                        colnames = colnames,
                        filter = filter,
                        style = style,
                        class = class,
                        container = container,
                        width = width,
                        height = height,
                        elementId = elementId,
                        fillContainer = fillContainer,
                        autoHideNavigation = autoHideNavigation,
                        selection = selection,
                        plugins = plugins,
                        editable = editable,
                        extensions = "Buttons",
                        options =
                                list(dom = "Blfrtip",
                                     buttons = c("copy", "csv","excel", "pdf", "print"),
                                     lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
                                     scrollX = scrollX,
                                     pageLength = pageLength,
                                     autoWidth = autoWidth
                                )
                )
                } else {
                        DT::datatable(
                                data = data,
                                caption = caption,
                                rownames = rownames,
                                colnames = colnames,
                                filter = filter,
                                style = style,
                                class = class,
                                container = container,
                                width = width,
                                height = height,
                                elementId = elementId,
                                fillContainer = fillContainer,
                                autoHideNavigation = autoHideNavigation,
                                selection = selection,
                                plugins = plugins,
                                editable = editable,
                                options =
                                        list(
                                             lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All")),
                                             scrollX = scrollX,
                                             pageLength = pageLength,
                                             autoWidth = autoWidth
                                        )
                        )
                }
        }

#' @title
#' Print Attribute Value Pairs as a Table
#' @description
#' Print user-provided attribute-value pairs in
#' a clean table format.
#'
#' @param ... Named list to print.
#' @param exclude_colnames Include "Attribute" and "Value" names?, Default: TRUE
#' @inheritParams kableExtra::kbl
#' @inheritParams kableExtra::kable_styling
#' @seealso
#'  \code{\link[rlang]{list2}},\code{\link[rlang]{parse_expr}}
#'  \code{\link[kableExtra]{kbl}},\code{\link[kableExtra]{kable_styling}}
#' @rdname print_list
#' @export
#' @importFrom rlang list2 parse_expr
#' @importFrom kableExtra kbl kable_styling

print_list <-
        function(...,
                 exclude_colnames = TRUE,
                 align = c("l", "l"),
                 full_width = FALSE,
                 position = "left",
                 htmltable_class = "lightable-minimal") {

                # Input must be present
                if (missing(...)) {
                        stop("'...' argument must be supplied as a named list.")
                }


                args <- unlist(rlang::list2(...))

                # All inputs must be named
                if (any(names(args) %in% c(""))) {

                        stop("names are required.")

                }

                params <- list()
                params[[1]] <- "    ~Attribute, ~Value"
                for (i in seq_along(args)) {

                        params[[i+1]] <-
                                sprintf("    '**%s**:', '%s'", names(args)[i], args[[i]])


                }
                params <- unlist(params)
                params <- paste(params, collapse = ",\n")

                fun_call <-
                        c("  tibble::tribble(",
                          params,
                          "  )") %>%
                        paste(collapse = "\n")

                parameter_df <-
                        eval(rlang::parse_expr(fun_call))

                if (exclude_colnames) {
                        colnames(parameter_df) <- NULL
                }

                kableExtra::kbl(parameter_df, align = align) %>%
                        kableExtra::kable_styling(full_width = full_width,
                                                  position = position,
                                                  htmltable_class = htmltable_class)

        }
