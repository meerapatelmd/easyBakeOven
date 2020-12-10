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


#' Make DESCRIPTION Imports
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
                             ignore.case = FALSE,
                             value = TRUE) %>%
                        stringr::str_replace_all(pattern = "(^.*?[()]{1})([a-zA-Z]{1}.*?)([,)]{1})(.*)",
                                                 replacement = "    \\2") %>%
                        unique() %>%
                        cat(sep = ",\n")

        }


#' Make URLS For DESCRIPTION
#' @description
#' Make the links to the Repo, GitHub Pages, and Issues Pages. To make these links in addition to the entire DESCRIPTION contents, see \code{\link{makeDescription}}.
#' @export
#' @rdname makeDescURL
#' @family make functions
#' @family DESCRIPTION functions


makeDescURL <-
        function (github_user,
                  repo) {

               makeDescriptionLinks(github_user = github_user,
                                             repo = repo)
}


#' Make DESCRIPTION
#'
#' @description
#' DESCRIPTION file in the given path is read and parsed into a dataframe using \code{\link{read_description}}, printed in the DESCRIPTION file format in the console. Additionally, the `Imports:` are read from the NAMESPACE file using \code{\link{read_namespace}}. If the DESCRIPTION file does not have an Imports section, it is returned in the console in red italics. Otherwise, it is returned in blue italics to still flag the output since this function does not compare whether the list of Imports match. This is up to the user as is updating the `Remotes:` section.  In a similar fashion, if either the `URL:` or `BugReports:` sections are missing, they are concatenated and returned in red italics. However, they are not returned in blue italics otherwise.
#'
#' @export
#' @rdname makeDescription
#' @family make functions
#' @family DESCRIPTION functions
#' @importFrom glitter get_gh_pages_url get_repo_url get_issues_page_url
#' @importFrom secretary redTxt italicize blueTxt

makeDescription <-
        function(path = getwd(),
                 github_user,
                 repo) {

                DESCRIPTION <- read_description(path = path)

                for (i in 1:nrow(DESCRIPTION)) {

                        cat(DESCRIPTION$headers[i], ": ", DESCRIPTION$value[i], "\n\n", sep = "")
                }


                output <- read_namespace(path = path)

                imports <- c(output$importFrom$pkg) %>%
                        unique()

                if ("import" %in% names(output)) {
                        imports <-
                                c(imports,
                                  output$import) %>%
                                unique()
                }

                imports <- paste(sprintf("    %s", imports), collapse = ",\n")


                if (!("Imports" %in% DESCRIPTION$headers)) {

                        c(Imports = sprintf("Imports: \n%s", imports)) %>%
                                secretary::redTxt() %>%
                                secretary::italicize() %>%
                                paste(collapse = "\n") %>%
                                cat()

                } else {

                        c(Imports = sprintf("Imports: \n%s", imports)) %>%
                                secretary::blueTxt() %>%
                                secretary::italicize() %>%
                                paste(collapse = "\n") %>%
                                cat()

                }

                cat("\n")

                if (!any("URL" %in% DESCRIPTION$headers)) {

                        gh_pages_url <- glitter::get_gh_pages_url(github_user = github_user,
                                                         repo = repo)
                        repo_url <- glitter::get_repo_url(github_user = github_user, repo = repo)

                        c(URL = sprintf("URL: %s/, %s/", gh_pages_url, repo_url)) %>%
                                secretary::redTxt() %>%
                                secretary::italicize() %>%
                                paste(collapse = "\n") %>%
                                cat()


                }

                cat("\n")

                if (!any("BugReports" %in% DESCRIPTION$headers)) {

                        issues_url <- glitter::get_issues_page_url(github_user = github_user,
                                                          repo = repo)
                        c(BugReports = sprintf("BugReports: %s/", issues_url)) %>%
                                secretary::redTxt() %>%
                                secretary::italicize() %>%
                                paste(collapse = "\n") %>%
                                cat()

                }

        }

#' @title
#' Make Links to Add to DESCRIPTION
#'
#' @inheritParams browse_gh
#'
#' @example inst/examples/packaging-make.R
#'
#' @rdname makeDescriptionLinks
#' @export

makeDescriptionLinks <-
        function(github_user,
                 repo) {

                gh_pages_url <- get_gh_pages_url(github_user = github_user,
                                                 repo = repo)
                repo_url <- get_repo_url(github_user = github_user,
                                         repo = repo)

                issues_url <- get_issues_page_url(github_user =
                                                          github_user,
                                                  repo = repo)

                c(URL = sprintf("URL: %s/, %s/", gh_pages_url, repo_url),
                  BugReports = sprintf("BugReports: %s/", issues_url)) %>%
                        paste(collapse = "\n") %>%
                        cat()

        }
