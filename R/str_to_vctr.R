#' @title
#' A Evaluate a Parsed Expression
#'
#' @description
#' For the inverse, see \code{\link{vector_to_string}}.
#'
#' @importFrom rlang parse_expr
#' @export
#' @example inst/examples/str_to_vctr.R
#' @rdname str_to_vctr


str_to_vctr <-
  function(string) {
    eval(rlang::parse_expr(string))
  }





#' @title
#' Convert a vector of values into a string
#'
#' @description
#' Store values neatly in an Excel or other dataframe for summary or record-keeping, but is in a format that can be readily parsed back to an expression if need be. This is important in cases where the names of columns want to be saved for a dataframe in a particular script to reference back or to QA later on. The string can be converted back to a vector by its counterpart function string_to_vector() in this package.
#' @export
#' @example inst/examples/str_to_vctr.R
#' @rdname vctr_to_str

vctr_to_str <-
  function(vector,
           quote = "'") {
    vector <- as.character(vector)

    # Enquoting the strings
    vector <- sprintf("%s%s%s", quote, vector, quote)

    # Adding commas and bordering with c()
    sprintf("c(%s)", paste(vector, collapse = ", "))
  }
