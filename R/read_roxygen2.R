#' @title
#' Read a Roxygen2 File Split By Function
#'
#' @export

split_function_file <-
        function(path) {
          text <- readLines(con = path, skipNul = TRUE, encoding = "UTF8")

          rox2_doc <- grepl(pattern = "^[#]{1}[']{1}",
                            x = text)

          output <- vector()
          for (i in seq_along(rox2_doc)) {

            if (i != 1) {
              previous <- rox2_doc[i-1]
              rox2_line <- rox2_doc[i]
              if (rox2_line == TRUE && previous == FALSE) {
                      output <-
                        c(output,
                          i)
              }
            }

          }

          start_index <- c(1, output)
          stop_index <- c(output-1, length(text))

          final <- list()
          for (i in seq_along(start_index)) {
            final[[i]] <-
              text[start_index[i]:stop_index[i]]

          }

          final


        }
