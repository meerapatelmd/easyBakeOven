#' @title
#' Create R Objects from Formal Argument Default Values
#'
#' @description
#' Declare formal arguments of a function along with their default values as R objects
#'
#' @rdname declareArgs
#' @family declaration functions
#' @export
#' @importFrom secretary typewrite magentaTxt
#' @importFrom purrr keep
#' @importFrom rlang is_missing


declareArgs <-
  function(fun) {
    Args <-
      formals(fun) %>%
      purrr::keep(~ !rlang::is_missing(.))

    nms <- names(Args)
    values <- unname(Args)


    for (i in seq_along(nms)) {
      assign(
        x = nms[i],
        value = values[[i]],
        envir = parent.frame()
      )

      secretary::typewrite(secretary::magentaTxt(nms[i]), "object created.")
    }
  }
