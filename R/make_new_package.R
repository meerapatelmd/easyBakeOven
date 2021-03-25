#' @title
#' Start a New Package
#'
#' @description
#' Starts a new package using a designated path to a local
#' directory, where the folder is created and the DESCRIPTION,
#' /R, LICENSE, NAMESPACE, and README are written. Additional
#' documentation such as the Code of Conduct can be generated
#' using `setup_package_docs()`.
#'
#' @inheritParams usethis::create_tidy_package
#'
#'
#' @rdname make_new_package
#' @export
#' @importFrom usethis create_tidy_package


make_new_package <-
  function(path,
           copyright_holder = NULL) {
    if (!dir.exists(paths = path)) {


      usethis::create_tidy_package(path = path,
                                   copyright_holder = copyright_holder)

    } else {
      secretary::typewrite(sprintf("Path '%s' already exists.", path))
    }
  }

#' @title
#' Setup Package Documentation
#'
#' @description
#' Adds the package doc `R/<package>-package.R`,
#' `README.Rmd`, Code of Conduct, and `NEWS.md` documents.
#' Spellcheck is also added usig this function.
#'
#' @inheritParams usethis::use_package_doc
#'
#' @param spellcheck_vignettes Spell check is automatically
#' enabled for the new package, but if TRUE, will also
#' spellcheck all rmd and rnw files in the vignettes/ folder.
#' @rdname setup_package_docs
#' @export
#' @importFrom usethis use_package_doc use_readme_rmd use_code_of_conduct use_news_md use_spell_check
#' @importFrom utils install.packages

setup_package_docs <-
  function(open = FALSE,
           spellcheck_vignettes = TRUE) {
    usethis::use_package_doc(open = open)
    usethis::use_readme_rmd(open = open)
    usethis::use_code_of_conduct()
    usethis::use_news_md(open = open)

    if (!("spelling" %in% installed.packages()[, "Package"])) {
      utils::install.packages("spelling")
    }
    usethis::use_spell_check(vignettes = spellcheck_vignettes)
  }

#' @title
#' Setup Tidy Git
#' @description
#' Setup both GitHub and tidy style settings in the usethis package.
#' @inheritParams usethis::use_github
#' @seealso
#'  \code{\link[usethis]{use_git}},\code{\link[usethis]{git_vaccinate}},
#'  \code{\link[usethis]{use_github}},\code{\link[usethis]{use_pipe}},
#'  \code{\link[usethis]{use_tibble}},
#'  \code{\link[usethis]{use_tidy_github_actions}}
#' @rdname setup_tidy_git
#' @export
#' @importFrom usethis use_git git_vaccinate use_github use_pipe use_tibble
#' use_tidy_eval use_tidy_style git_protocol

setup_tidy_git <-
  function(initial_commit_message = "initial commit",
           organisation = NULL,
           private = FALSE,
           protocol = usethis::git_protocol(),
           host = NULL,
           steps = c("use_git",
                     "git_vaccinate",
                     "use_github",
                     "use_pipe",
                     "use_tibble",
                     "use_tidy_eval",
                     "use_tidy_style")) {

    if ("use_git" %in% steps) {
      usethis::use_git(message = initial_commit_message)
    }

    if ("git_vaccinate" %in% steps) {
    usethis::git_vaccinate()
    }

    if ("use_github" %in% steps) {
    usethis::use_github(
      organisation = organisation,
      private = private,
      protocol = protocol,
      host = host
    )
    }

    if ("use_pipe" %in% steps) {
    usethis::use_pipe(export = FALSE)
    }

    if ("use_tibble" %in% steps) {
    usethis::use_tibble()
    }

    if ("use_tidy_eval" %in% steps) {
    usethis::use_tidy_eval()
    }

    if ("use_tidy_style" %in% steps) {
    usethis::use_tidy_style(strict = TRUE)
    }
  }
