









start_project <-
        function(path,
                 open = FALSE,
                 spellcheck_vignettes = TRUE) {
                usethis::use_package_doc(open = open)
                usethis::use_readme_md(open = open)
                usethis::use_news_md(open = open)

                if (!("spelling" %in% installed.packages()[, "Package"])) {
                        utils::install.packages("spelling")
                }
                usethis::use_spell_check(vignettes = spellcheck_vignettes)
                usethis::git_vaccinate()
                usethis::use_pipe(export = TRUE)
                usethis::use_tibble()
                usethis::use_tidy_eval()
                usethis::use_tidy_style(strict = TRUE)
        }
