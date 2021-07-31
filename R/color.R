color_to_hex <-
        function(x,
                 alpha = FALSE) {

                rgb_values <-
                        grDevices::col2rgb(col= x,
                                alpha = alpha)[,1] %>%
                        as.list()


                grDevices::rgb(red = rgb_values$red,
                    green = rgb_values$green,
                    blue = rgb_values$blue,
                    maxColorValue = 255)

        }


colors_hex_map <-
        function() {
                sapply(colors(), color_to_hex)
        }

hex_to_color <-
        function(hex) {

                names(colors_hex_map()[hex %in% colors_hex_map()])

        }

hex_colors_map <-
        function(x) {

                x <- colors_hex_map()
                output <- names(x)
                names(output) <- x
                output


        }


base_colors <-
        function() {

                all_colors_df <-
                        tibble(
                                all_colors = colors())


                r_df <-
                        all_colors_df %>%
                        extract(col = all_colors,
                                into = c("base_color_y", "level"),
                                regex = "(^.*?)([0-9]{1,}$)",
                                remove = FALSE) %>%
                        dplyr::filter(!is.na(base_color_y),
                                      !is.na(level)) %>%
                        group_by(base_color_y) %>%
                        summarize(levels = length(unique(all_colors))) %>%
                        ungroup()


                l_df <-
                        all_colors_df %>%
                        rubix::filter_at_grepl(
                                col = all_colors,
                                grepl_phrase = "[0-9]{1,}$",
                                evaluates_to = FALSE) %>%
                        rename(base_color_x = all_colors)


                left_join(
                        x = l_df,
                        y = r_df,
                        by = c("base_color_x" = "base_color_y"),
                        keep = TRUE) %>%
                        transmute(
                                base_color = coalesce(base_color_x, base_color_y),
                                has_shades = !is.na(base_color_y),
                                shade_levels = levels)

        }



# Adapted from scales::show_col. Labels are color names instead
# of hex code.

view_colors <-
        function (..., labels = TRUE, borders = NULL, cex_label = 0.5,
                  ncol = NULL) {

                colours <- unlist(rlang::list2(...))

                n <- length(colours)
                ncol <- ncol %||% ceiling(sqrt(length(colours)))
                nrow <- ceiling(n/ncol)
                colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
                colours <- matrix(colours, ncol = ncol, byrow = TRUE)
                old <- par(pty = "s", mar = c(0, 0, 0, 0))
                on.exit(par(old))
                size <- max(dim(colours))
                plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
                     axes = FALSE)
                rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours),
                     col = colours, border = borders)
                if (labels) {
                        text(x = col(colours) - 0.5,
                             y = -row(colours) + 0.5,
                             labels = colours,
                             cex = cex_label,
                             col = "black")
                }
        }


view_color_shades <-
        function(...) {


                colours <- unlist(rlang::list2(...))
                colours <-
                        stringr::str_remove_all(colours,
                                       pattern = "[0-9]{1,}$")

                all_colours_with_shades <-
                        base_colors() %>%
                        dplyr::filter(has_shades == TRUE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()

                all_colours_without_shades <-
                        base_colors() %>%
                        dplyr::filter(has_shades == FALSE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()


                final_colours <-
                        vector(mode = "list",
                               length = length(colours))
                names(final_colours) <- colours

                for (colour in colours) {

                        if (colour %in% all_colours_with_shades) {
                                final_colours[[colour]] <-
                                c(colour,
                                  sprintf("%s%s", colour, 1:4))

                        } else {

                                final_colours[[colour]] <-
                                        c(colour,
                                          rep("white", 4))


                        }

                }

                final_colours <-
                        unname(unlist(final_colours))

#
#                 colours_with_shades2 <-
#                         vector(mode = "list",
#                                length = length(colours_with_shades))
#                 names(colours_with_shades2) <-
#                         colours_with_shades
#
#                 for (colour_with_shades in colours_with_shades) {
#                         colours_with_shades2[[colour_with_shades]] <-
#                                 c(colour_with_shades,
#                                 sprintf("%s%s", colour_with_shades, 1:4))
#
#                 }
#
#                 colours_with_shades3 <-
#                         unlist(colours_with_shades2) %>%
#                         unname()
#
#                 if (length(colours_without_shades)>0) {
#                         colours_without_shades2 <-
#                                 vector(mode = "list",
#                                        length = length(colours_without_shades))
#                         names(colours_with_shades2) <- colours_without_shades
#
#
#                         for (colour_without_shade in colours_without_shades) {
#
#                                 colours_without_shades2[[colour_without_shade]] <-
#                                         c(colour_without_shade,
#                                           rep("white", 4))
#
#
#
#                         }
#
#                         colours_without_shades3 <-
#                                 unlist(colours_without_shades2) %>%
#                                 unname()
#
#
#                         final_colours <-
#                                 c(colours_with_shades3,
#                                   colours_without_shades3)
#
#
#                 } else {
#                         final_colours <-
#                                 colours_with_shades3
#
#                 }
#
#                 # view_colors(final_colours,
#                 #             ncol = 5)


                borders <- NULL
                cex_label <- 0.5
                labels <- TRUE
                n <- length(final_colours)
                ncol <- 5
                nrow <- ceiling(n/ncol)
                final_colours <- c(final_colours, rep(NA, nrow * ncol - length(final_colours)))
                final_colours <- matrix(final_colours, ncol = ncol, byrow = TRUE)
                old <- par(pty = "s", mar = c(0, 0, 0, 0))
                on.exit(par(old))
                size <- max(dim(final_colours))
                plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "",
                     axes = FALSE)
                rect(col(final_colours) - 1, -row(final_colours) + 1, col(final_colours), -row(final_colours),
                     col = final_colours, border = borders)

                if (labels) {

                        final_labels <-
                                final_colours

                        final_labels[final_labels == "white"] <- ""

                        text(x = col(final_colours) - 0.5,
                             y = -row(final_colours) + 0.5,
                             labels = final_labels,
                             cex = cex_label,
                             col = "black")
                }



        }

assign_colors_by <-
        function(x) {

                color_assignment <-
                        unique(x)

                names(color_assignment) <- colorspace::diverge_hcl(n = length(color_assignment))

                fct_recode(factor(x),
                           !!!color_assignment) %>%
                        as.character()

        }


add_html_color <-
        function(text,
                 color) {

                glue::glue('<span style="color: {color};">{text}</span>')


        }

map_colors <-
        function(x,
                 color_assignment) {

                color_assignment2 <- names(color_assignment)
                names(color_assignment2) <- color_assignment

                fct_recode(factor(x),
                           !!!color_assignment2) %>%
                        as.character()

        }


assign_colors <-
        function(x,
                 taken_colors,
                 shade_level = 2,
                 seed = 100,
                 exclude_color_regex) {

                if (!missing(taken_colors)) {
                        taken_colors <-
                                taken_colors[!is.na(taken_colors)]
                        taken_colors <-
                                unique(taken_colors)
                        base_taken_colors <-
                                str_remove_all(taken_colors,
                                               pattern = "[0-9]{1,}$")
                } else {
                        base_taken_colors <- vector(mode = "character")
                }

                available_base_colors <-
                        base_colors() %>%
                        dplyr::filter(has_shades == TRUE) %>%
                        dplyr::filter(!(base_color %in% base_taken_colors)) %>%
                        transmute(base_color = sprintf("%s%s", base_color, shade_level)) %>%
                        unlist() %>%
                        unname() %>%
                        grep(pattern = "black|white|ivory|snow|beige|gray|grey",
                             value = TRUE,
                             invert = TRUE)

                if (!missing(exclude_color_regex)) {

                        available_base_colors <-
                                grep(pattern = exclude_color_regex,
                                     x = available_base_colors,
                                     value = TRUE,
                                     invert = TRUE)
                }



                x <- unique(x)
                x <- x[!is.na(x)]

                output <-
                        vector(mode = "list",
                               length = length(x))
                names(output) <- x

                set.seed(seed = seed)

                for (i in seq_along(x)) {

                        output[[x[i]]] <-
                                sample(available_base_colors,
                                       size = 1)

                        available_base_colors <-
                                available_base_colors[!(available_base_colors %in% output[[x[i]]])]

                }

                output


        }
