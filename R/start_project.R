#' @title
#' Start a Package Project
#' @description
#' Similar suite of functions as \code{\link{make_new_package}} that can occur every time a package is started (i.e. not a one-time setup function.)
#'
#' @inheritParams make_new_package
#' @inheritParams usethis::create_package
#' @inheritParams usethis::use_readme_md
#' @inheritParams usethis::use_github
#' @seealso
#'  \code{\link[usethis]{use_package_doc}},\code{\link[usethis]{use_readme_rmd}},\code{\link[usethis]{use_news_md}},\code{\link[usethis]{use_spell_check}},\code{\link[usethis]{git_vaccinate}}
#'  \code{\link[utils]{install.packages}}
#' @rdname start_pkg_proj
#' @export
#' @importFrom usethis use_package_doc use_readme_md use_news_md use_spell_check git_vaccinate
#' @importFrom utils install.packages
start_pkg_proj <-
  function(path,
           open = FALSE,
           spellcheck_vignettes = TRUE) {

          files <- list.files(path = path)
          package <- basename(path)

          package_r_file <- sprintf("%s/R/%s-package.R", path, package)
          readme_file <- sprintf("%s/README.md", path)
          news_file <- sprintf("%s/NEWS.md", path)

          if (!file.exists(package_r_file)) {
    usethis::use_package_doc(open = open)

          }

          if (!file.exists(readme_file)) {
    usethis::use_readme_md(open = open)

          }

          if (!file.exists(news_file)) {
    usethis::use_news_md(open = open)

          }

    if (!("spelling" %in% installed.packages()[, "Package"])) {
      utils::install.packages("spelling")
    }
    usethis::use_spell_check(vignettes = spellcheck_vignettes)
    usethis::git_vaccinate()
  }
