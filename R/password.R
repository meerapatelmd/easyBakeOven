#' @title
#' Generate Password
#'
#' @rdname generate_password
#' @export


generate_password <-
  function(words = c("adjective", "noun", "adverb", "verb")) {

    paste(process_password_words(!!!words),
          collapse = "")


  }


#' @import purrr
#' @keywords internal


process_password_words <-
  function(...) {

    lu <-
    list(
      adjective =
    system.file(package = "easyBakeOven",
                "password",
                "adjectives.txt"),
    noun =
      system.file(package = "easyBakeOven",
                  "password",
                  "nouns.txt"),
    adverb =
      system.file(package = "easyBakeOven",
                  "password",
                  "adverbs.txt"),
    verb =
      system.file(package = "easyBakeOven",
                  "password",
                  "verbs.txt"),
    other =
      system.file(package = "easyBakeOven",
                  "password",
                  "other.txt"))


    dictionaries <-
    lu %>%
      purrr::map(readLines) %>%
      purrr::set_names(names(lu))


    Args <- unlist(rlang::list2(...))

    output <- vector()

    for (Arg in Args) {

      output <-
        c(output,
      sample(x = dictionaries[[Arg]],
             size = 1))

    }


    output


  }
