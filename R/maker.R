#' @title
#' Make List
#' @param list List.
#' @rdname make_list
#' @export
make_list <-
  function(list) {

    output <- list()

    for (i in 1:length(list)) {

      output[[i]] <-
        sprintf("  `%s` = c(%s)",
                names(list)[i],
                paste(sprintf("'%s'", list[[i]]), collapse = ", "))

    }

    output <- unlist(output)
    output <-
      paste(output, collapse = ",\n")

    output <-
      c("list(",
        output,
        ")")

    cat(output,
        sep = "\n")

  }

#' @title
#' Make Tribble Expression
#' @param data Data to convert to expression.
#' @param quote How strings will be flanked, Default: '''
#' @rdname make_tribble
#' @export
#' @importFrom dplyr mutate_if
make_tribble <-
  function(data, quote = "'") {

    data <-
      data %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::mutate_if(is.character, ~sprintf("%s%s%s",
                                              quote,
                                              ., quote))

    col_output <-
      sprintf("`%s`", colnames(data))
    col_output <-
      paste0("~", col_output)
    col_output <-
      paste0(col_output, ",")
    col_output <-
      paste(col_output, collapse = " ")
    col_output <-
      paste0("      ", col_output)

    row_output <- list()
    for (i in 1:nrow(data)) {
      row_vector <-
          data[i, ] %>%
          unlist()

      if (i != nrow(data)) {
        #Add comma
        row_vector <-
          paste0(row_vector, ",") %>%
          paste(collapse = "")
      } else {
        row_vector <-
          paste(row_vector, collapse = ",")
      }
      # Collapse to preserve the rows
      row_output[[i]] <-
        row_vector
    }

    row_output <- unlist(row_output)
    row_output <- paste0("      ", row_output)

    output <-
      c("    tibble::tribble(",
        col_output,
        row_output,
        "    )")
    output <-
      paste(output,
            collapse = "\n")


    cat(output)
  }
