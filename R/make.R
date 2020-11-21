#' @title
#' Make a Formal Argument Skeleton with Default Values
#'
#' @description
#' Retrieve the formal arguments with the default values to copy-and-paste as formal arguments of another function. To copy-and-paste formal arguments as part of an internal function, see \code{\link{makeInternalArgs}}.
#'
#' @param fun  Function object.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'        makeDefaultArgs(read.csv)
#'  }
#' }
#'
#' @rdname makeDefaultArgs
#'
#' @importFrom stringr str_remove_all
#' @export

makeDefaultArgs <-
        function(fun) {

                nms <- names(formals(fun))
                values <- unname(formals(fun))

                values2 <- vector()
                for (i in seq_along(values)) {
                        values2 <-
                                c(values2,
                                  deparse(values[[i]]))
                }

                mapply(paste0, nms, " = ", values2)  %>%
                        stringr::str_remove_all(pattern = " [=]{1} $") %>%
                        paste(collapse = ",\n") %>%
                        cat()

        }

#' @title
#' Make a Formal Argument Skeleton with Default Values
#'
#' @description
#' Retrieve the formal arguments assigned to itself  to copy-and-paste as an internal function call. To copy-and-paste formal arguments with their default values, see \code{\link{makeDefaultArgs}}.
#'
#' @param fun PARAM_DESCRIPTION
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'        makeInternalArgs(read.csv)
#'  }
#' }
#'
#' @rdname makeInternalArgs
#'
#' @export

makeInternalArgs <-
        function(fun) {

                nms <- names(formals(fun))
                values <- unname(formals(fun))

                mapply(paste0, nms, " = ", nms) %>%
                        paste(collapse = ",\n") %>%
                        cat()

        }

#' Make Args
#' @export
#' @rdname makeArgs
#' @family make functions

makeArgs <-
        function(fun) {

                makeDefaultArgs(fun = fun)

                cat("\n\n")

                makeInternalArgs(fun = fun)

                cat("\n\n")

        }


#' Make Arg Declaration
#' @description
#' To automatically assign argument objects with the default value, see \code{\link{declareArgs}}
#'
#' @importFrom rlang is_missing
#' @importFrom purrr keep
#' @export
#' @rdname makeArgDeclaration
#' @family make functions

makeArgDeclaration <-
        function(fun) {

                Args <-
                        formals(fun) %>%
                        purrr::keep(~ !rlang::is_missing(.))

                nms <- names(Args)
                values <- unname(Args)

                values2 <- vector()
                for (i in seq_along(values)) {
                        values2 <-
                                c(values2,
                                  deparse(values[[i]]))
                }

                mapply(paste0, nms, " <- ", values2) %>%
                        paste(collapse = "\n") %>%
                        cat()

        }


#' Make Arg Declaration
#' @description
#' To automatically assign argument objects with the default value, see \code{\link{declareArgs}}
#'
#' @importFrom rlang is_missing
#' @importFrom purrr keep
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all
#' @export
#' @rdname makeImports
#' @family make functions
#' @family DESCRIPTION functions

makeImports <-
        function() {

                cat("Imports:\n")

                readr::read_lines(file = "NAMESPACE") %>%
                        grep(pattern = "import",
                             ignore.case = TRUE,
                             value = TRUE) %>%
                        stringr::str_replace_all(pattern = "(^.*?[()]{1})([a-zA-Z]{1}.*?)([,)]{1})(.*)",
                                                 replacement = "    \\2") %>%
                        cat(sep = ",\n")

        }
