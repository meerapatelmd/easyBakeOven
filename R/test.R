list_all_variables <-
        function(file) {


                rmdLines <- readLines(con = file)
                rmdLines <-
                        grep(pattern = "[<]{3}.*?[>]{3}",
                             x = rmdLines,
                             value = TRUE) %>%
                        stringr::str_replace_all(
                                pattern = "^.*?[<]{3}(.*?)[>]{3}.*$",
                                replacement = "\\1"
                        )

                unique(rmdLines)

        }




list_missing_variables <-
        function(file) {


                rmdLines <- readLines(con = file)

                safely.glue <-
                        safely(glue::glue)

                output <- list()
                for (rmdLine in rmdLines) {
                        output[[length(output)+1]] <-
                        safely.glue(
                                rmdLine,
                                .open = "<<<",
                                .close = ">>>"
                        )
                }


                output %>%
                        transpose() %>%
                        pluck("error") %>%
                        keep(~!is.null(.)) %>%
                        transpose() %>%
                        pluck("message") %>%
                        unlist() %>%
                        stringr::str_replace_all(pattern = "(object [']{1})(.*?)([']{1}.*$)",
                                                   replacement = "\\2")


        }
