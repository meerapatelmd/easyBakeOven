#' @title
#' Start a New Package
#' @description
#' Starts a new package using a designated path to a local directory, where the folder is created, README.md and NEWS.md files are added, git is initialized and linked to a remote of the same name.
#'
#' @inheritParams usethis::create_package
#' @inheritParams usethis::use_readme_md
#' @inheritParams usethis::use_github
#' @param spellcheck_vignettes Spell check is automatically enabled for the new package, but if TRUE, will also spell check all rmd and rnw files in the vignettes/ folder.
#' @seealso
#'  \code{\link[usethis]{create_package}},\code{\link[usethis]{proj_activate}},\code{\link[usethis]{use_readme_rmd}},\code{\link[usethis]{use_news_md}},\code{\link[usethis]{use_git}}
#' @rdname make_new_package
#' @export
#' @importFrom usethis create_package proj_activate use_readme_md use_news_md use_git


make_new_package <-
  function(path,
           open = FALSE,
           spellcheck_vignettes = TRUE,
           initial_commit_message = "Initial commit",
           organisation = NULL,
           private = FALSE,
           protocol = usethis::git_protocol(),
           host = NULL) {
    if (!dir.exists(paths = path)) {
      usethis::create_tidy_package(path = path)

      current_wd <- getwd()
      on.exit(setwd(dir = current_wd))

      usethis::proj_activate(path = path)
      usethis::use_package_doc(open = open)
      usethis::use_readme_md(open = open)
      usethis::use_news_md(open = open)

      if (!("spelling" %in% installed.packages()[, "Package"])) {
        utils::install.packages("spelling")
      }
      usethis::use_spell_check(vignettes = spellcheck_vignettes)
      usethis::use_git(message = initial_commit_message)
      usethis::git_vaccinate()
      usethis::use_github(
        organisation = organisation,
        private = private,
        protocol = protocol,
        host = host
      )
      usethis::use_pipe(export = TRUE)
      usethis::use_tibble()
      usethis::use_tidy_eval()
      usethis::use_tidy_style(strict = TRUE)
    } else {
      secretary::typewrite(sprintf("Path '%s' already exists.", path))
    }
  }
