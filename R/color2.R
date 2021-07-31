unshaded_base_colors <-
        function() {
                base_colors() %>%
                        dplyr::filter(has_shades == FALSE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }


shaded_base_colors <-
        function() {
                base_colors() %>%
                        dplyr::filter(has_shades == TRUE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }



view_colors_regex <-
        function(pattern,
                 invert = FALSE,
                 labels = TRUE,
                 borders = NULL,
                 cex_label = 0.5,
                 ncol = NULL) {

                colours <-
                        grep(pattern = pattern,
                             x = base_colors() %>%
                                     select(base_color) %>%
                                     unlist() %>%
                                     unname(),
                             value = TRUE,
                             invert = invert)


                view_colors(
                        colours,
                        labels = labels,
                        borders = borders,
                        cex_label = cex_label,
                        ncol = ncol
                )



        }


view_color_shades_regex <-
        function(pattern,
                 invert = FALSE) {


                colours <-
                        grep(pattern = pattern,
                             x = base_colors() %>%
                                     select(base_color) %>%
                                     unlist() %>%
                                     unname(),
                             value = TRUE,
                             invert = invert)

                view_color_shades(colours)


        }
