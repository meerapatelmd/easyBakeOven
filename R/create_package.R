#' @title
#' Start a New Package
#' @description
#' Starts a new package using a designated path to a local directory, where the folder is created, README.md and NEWS.md files are added, git is initialized and linked to a remote of the same name.
#'
#' @inheritParams usethis::create_package
#' @inheritParams usethis::use_readme_md
#' @inheritParams usethis::use_github
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[usethis]{create_package}},\code{\link[usethis]{proj_activate}},\code{\link[usethis]{use_readme_rmd}},\code{\link[usethis]{use_news_md}},\code{\link[usethis]{use_git}}
#' @rdname make_new_package
#' @export
#' @importFrom usethis create_package proj_activate use_readme_md use_news_md use_git


make_new_package <-
        function(path,
                 open = FALSE,
                 initial_commit_message = "Initial commit",
                 organisation = NULL,
                 private = FALSE,
                 protocol = usethis::git_protocol(),
                 host = NULL) {

                usethis::create_package(path = path)

                current_wd <- getwd()
                on.exit(setwd(dir = current_wd))

                usethis::proj_activate(path = path)
                usethis::use_readme_md(open = open)
                usethis::use_news_md(open = open)
                usethis::use_git(message = initial_commit_message)
                usethis::use_github(organisation = organisation,
                                    private = private,
                                    protocol = protocol,
                                    host = host)
        }
