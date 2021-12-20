#' @title
#' Generate Password
#'
#' @param words Type of word in chronological order. Can be
#' 1 of "adjective", "noun", "adverb", "verb", or "other".
#' @param collapse Passed to `collapse` in `paste()`.
#' @param numbers  Integer of number count to be added to password.
#' @param symbols  Integer of symbol count to be added to password.
#' @param unique   Should every character within the symbol be unique? This
#' includes the numbers and symbols that are added to the password.
#' @param verbose  Only applies when `unique` is set to TRUE. If TRUE, each generated password is printed to the console prior to undergoing uniqueness testing. If the password passes the uniqueness test, then it is printed to the console along with the time it took for the password to be generated.
#'
#' @details
#' This function will throw an error if the length of `words` is greater than 2
#' along with a `collapse` value that is not `""`, but `unique` is set to `TRUE`.
#' The generated password will be in the order of 1. words, 2. numbers, and
#' 3. symbols and cannot be altered.
#'
#'
#' @rdname generate_password
#' @export


generate_password <-
  function(words = c("adjective", "noun"),
           collapse = "-",
           numbers = 0,
           symbols = 0,
           unique = FALSE,
           verbose = TRUE) {


    if (unique & length(words)>2 & collapse != "") {

      stop("Cannot generate a password where each character is unique if more than two words are required are separated by a value (`collapse`) that will indefinitely be repeated. Either change `unique` to FALSE, change `collapse` to '\"\"', or reduce the length of words to 2 or less.")

    }



    if (!unique) {

      out <-
        paste(process_password_words(!!!words),
              collapse = collapse)


      numbers_out <-
        sample(x = 1:9,
               size = numbers,
               replace = !unique)
      numbers_out <-
        paste(numbers_out, collapse = "")


      symbols_out <-
      sample(x = c("$", "!", "*", "&", "^"),
             size = symbols,
             replace = !unique)
      symbols_out <-
        paste(symbols_out, collapse = "")


      out <-
        c(out,
          numbers_out,
          symbols_out)

      out <-
        paste(out,
              collapse = "")

      print(out)
      invisible(out)

    } else {



      proceed <- TRUE

      start_dt <- Sys.time()

      while (proceed) {

        out_i <-
          paste(process_password_words(!!!words),
                collapse = collapse)


        numbers_out <-
          sample(x = 1:9,
                 size = numbers,
                 replace = unique)
        numbers_out <-
          paste(numbers_out, collapse = "")


        symbols_out <-
          sample(x = c("$", "!", "*", "&", "^"),
                 size = symbols,
                 replace = unique)
        symbols_out <-
          paste(symbols_out, collapse = "")


        out_i <-
          c(out_i,
            numbers_out,
            symbols_out)

        out_i <-
          paste(out_i,
                collapse = "")

        if (verbose) {

          cli::cli_inform(message = "Testing {out_i}")


        }

        unique_character_ct <-
          strsplit(out_i,
                   split = "") %>%
          unlist() %>%
          unique() %>%
          length()


        total_character_ct <-
          nchar(out_i)

        if (unique_character_ct == total_character_ct) {

          if (verbose) {
            cli::cli_alert_success(text = "Success: '{out_i}'")
            msg1 <- prettyunits::time_ago(start_dt)
            cli::cli_alert_info(text = msg1)

          } else {

            print(out_i)
            invisible(out_i)


          }


          break

        }


      }




    }


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
