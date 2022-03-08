#' @title
#' Color to Hex
#' @description
#' Get the hex code for a color given by its name.
#'
#' @inheritParams grDevices::col2rgb
#' @param x Color name as string.
#' @return
#' Vector of length 1 or greater of hex codes matched to the RGB derived
#' from the color name.
#'
#' @examples
#' color_to_hex("blue")
#' @seealso
#'  \code{\link[grDevices]{col2rgb}},\code{\link[grDevices]{rgb}}
#' @rdname color_to_hex
#' @export
#' @importFrom grDevices col2rgb rgb
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


#' @title
#' Map Between Base R Colors and Hex Codes
#'
#' @description
#' All colors returned by base::colors() are
#' mapped to their corresponding hex codes.
#'
#' @return
#' Vector of hexadecimal codes named by the source
#' color name.
#'
#' @rdname colors_hex_map
#' @export
colors_hex_map <-
        function() {
                sapply(colors(), color_to_hex)
        }

#' @title
#' Lookup the Color Name of a Hex Code
#'
#'
#' @rdname hex_to_color
#' @export
hex_to_color <-
        function(hex) {

                names(colors_hex_map()[hex %in% colors_hex_map()])

        }

#' @title
#' Map Between Hex Codes and Base R Colors
#'
#' @description
#' The inverse map of all colors returned by base::colors() are
#' mapped to their corresponding hex codes.
#'
#' @return
#' Vector of color names named by their corresponding hexadecimal code.
#'
#' @rdname hex_colors_map
#' @export
hex_colors_map <-
        function(x) {

                x <- colors_hex_map()
                output <- names(x)
                names(output) <- x
                output


        }


#' @title
#' Base Colors
#' @rdname base_colors
#' @export
#' @importFrom dplyr filter
#' @importFrom rubix filter_at_grepl
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


                output <-
                left_join(
                        x = l_df,
                        y = r_df,
                        by = c("base_color_x" = "base_color_y"),
                        keep = TRUE) %>%
                        transmute(
                                base_color = coalesce(base_color_x, base_color_y),
                                color_type = ifelse(is.na(levels), "Unshaded", "Shaded"),
                                shade_levels = ifelse(is.na(levels), 0, levels)) %>%
                        split(.$color_type) %>%
                        purrr::map(select, -color_type)

                output$Shaded <-
                        output$Shaded %>%
                        split(.$shade_levels) %>%
                        purrr::map(dplyr::select, -shade_levels)

                output$Unshaded <-
                        output$Unshaded %>%
                        dplyr::select(-shade_levels)

                return(output)

        }




# Adapted from scales::show_col. Labels are color names instead
# of hex code.

#' @title
#' View Colors in the Plots Pane
#' @param ... Color names or hex codes.
#' @param labels If TRUE, the plot is labelled with the color name. Default: TRUE
#' @param borders Color for rectangle border(s) that is passed to `rect()`, Default: NULL
#' @param cex_label Numeric character expansion factor; multiplied by par("cex") yields the final character size. NULL and NA are equivalent to 1.0, Default: 0.5
#' @param ncol How wide the plot should be, Default: NULL
#' @seealso
#'  \code{\link[rlang]{list2}}
#' @rdname view_colors
#' @export
#' @importFrom rlang list2
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

#' @title
#' View Colors in the Plots Pane
#' @param ... Color names or hex codes.
#' @param labels Custom labels other than the color name.
#' @param borders Color for rectangle border(s) that is passed to `rect()`, Default: NULL
#' @param cex_label Numeric character expansion factor; multiplied by par("cex") yields the final character size. NULL and NA are equivalent to 1.0, Default: 0.5
#' @param ncol How wide the plot should be, Default: NULL
#' @seealso
#'  \code{\link[rlang]{list2}}
#' @rdname view_labelled_colors
#' @export
#' @importFrom cli cli_alert_warning
#' @importFrom rlang list2
view_labelled_colors <-
        function (..., labels = NULL, borders = NULL, cex_label = 0.5,
                  ncol = NULL) {

                arg_length <- length(unlist(rlang::list2(...)))
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

                rect(col(colours) - 1,
                     -row(colours) + 1,
                     col(colours),
                     -row(colours),
                     col = colours,
                     border = borders)

                if (!is.null(labels)) {

                        if (length(labels) == arg_length) {
                                text(x = col(colours) - 0.5,
                                     y = -row(colours) + 0.5,
                                     labels = labels,
                                     cex = cex_label,
                                     col = "black")
                        } else {

                                cli::cli_alert_warning("There are {length(labels)} `labels` and {length(colours)} color{?s}. `labels` must be the same length as colors. No labels are applied.")
                        }
                }
        }

#' @title
#' View Each Color and Its Shades
#' @inheritParams view_colors
#' @param ... Base names of colors from `base_colors()`
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[stringr]{str_remove}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#' @rdname view_color_shades
#' @export
#' @importFrom rlang list2
#' @importFrom stringr str_remove_all
#' @importFrom dplyr filter select
view_color_shades <-
        function(...,
                 labels = TRUE, borders = NULL, cex_label = 0.5,
                 ncol = NULL) {


                colours <- unlist(rlang::list2(...))
                colours <-
                        stringr::str_remove_all(colours,
                                       pattern = "[0-9]{1,}$")

                all_colours_with_shades <-
                        base_colors()$Shaded$`4` %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()

                all_colours_without_shades <-
                        base_colors()$Unshaded %>%
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




#' @title
#' Use Colorspace Palettes for Map Assignment
#' @description
#' Instead of having to manually determine a map assignment,
#' a colorspace palette can be used to auto-assign to a given
#' vector.
#' @param x Vector to map. All vectors are converted to character
#' before mapping.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   auto_map_to_color(x = as.character(x)) %>%
#'   view_colors()
#'  }
#' }
#' @seealso
#'  \code{\link[colorspace]{hcl_palettes}}
#' @rdname auto_map_to_palette
#' @export
#' @import colorspace
auto_map_to_palette <-
        function(x,
                 colorspace_palette = "diverge_hcl",
                 ...) {

                color_assignment <-
                        unique(as.character(x))

                names(color_assignment) <-
                        eval(rlang::parse_expr(glue::glue("colorspace::{colorspace_palette}(n = length(color_assignment))")))

                fct_recode(factor(x),
                           !!!color_assignment) %>%
                        as.character()

        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[colorspace]{hcl_palettes}}
#' @rdname assign_colors_by
#' @export
#' @importFrom colorspace diverge_hcl
assign_colors_by <-
        function(x) {

                color_assignment <-
                        unique(x)

                names(color_assignment) <- colorspace::diverge_hcl(n = length(color_assignment))

                fct_recode(factor(x),
                           !!!color_assignment) %>%
                        as.character()

        }


#' @title
#' Randomly Select Colors
#' @param x Vector to map.
#' @param taken_colors (Optional) Colors to exclude from selection because they have
#' already been taken.
#' @param shade_level Shade level from 1 to 4, Default: 2
#' @param seed Default: 100
#' @param exclude_color_regex (Optional) Regex to exclude a subset of colors.
#' @examples
#' auto_map_random_colors(x = c("A", "B", "C))
#' @seealso
#'  \code{\link[dplyr]{filter}}
#' @rdname auto_map_random_colors
#' @export
#' @importFrom dplyr filter
auto_map_random_colors <-
        function(x,
                 taken_colors,
                 shade_level = 2,
                 seed = 100,
                 exclude_color_regex = "black|white|ivory|snow|beige|gray|grey") {

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
                        base_colors()$Shaded$`4` %>%
                        dplyr::filter(!(base_color %in% base_taken_colors)) %>%
                        transmute(base_color = sprintf("%s%s", base_color, shade_level)) %>%
                        unlist() %>%
                        unname() %>%
                        grep(pattern = exclude_color_regex,
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


# From wikipedia on X11
color_groups <-
        list(
        `pink` = c('mediumvioletred', 'deeppink', 'palevioletred', 'hotpink', 'lightpink', 'pink'),
        `red` = c('darkred', 'red', 'firebrick', 'indianred', 'lightcoral', 'salmon', 'darksalmon', 'lightsalmon'),
        `orange` = c('orangered', 'tomato', 'darkorange', 'coral', 'orange'),
        `yellow` = c('darkkhaki', 'gold', 'khaki', 'peachpuff', 'yellow', 'palegoldenrod', 'moccasin', 'papayawhip', 'lightgoldenrodyellow', 'lemonchiffon', 'lightyellow'),
        `brown` = c('maroon', 'brown', 'saddlebrown', 'sienna', 'chocolate', 'darkgoldenrod', 'peru', 'rosybrown', 'goldenrod', 'sandybrown', 'tan', 'burlywood', 'wheat', 'navajowhite', 'bisque', 'blanchedalmond', 'cornsilk'),
        `green` = c('darkgreen', 'green', 'darkolivegreen', 'forestgreen', 'seagreen', 'olivedrab', 'mediumseagreen', 'limegreen', 'springgreen', 'mediumspringgreen', 'darkseagreen', 'mediumaquamarine', 'yellowgreen', 'lawngreen', 'chartreuse', 'lightgreen', 'greenyellow', 'palegreen'),
        `cyan` = c('darkcyan', 'lightseagreen', 'cadetblue', 'darkturquoise', 'mediumturquoise', 'turquoise', 'cyan', 'aquamarine', 'paleturquoise', 'lightcyan'),
        `blue` = c('navy', 'darkblue', 'mediumblue', 'blue', 'midnightblue', 'royalblue', 'steelblue', 'dodgerblue', 'deepskyblue', 'cornflowerblue', 'skyblue', 'lightskyblue', 'lightsteelblue', 'lightblue', 'powderblue'),
        `purple` = c('purple', 'darkmagenta', 'darkviolet', 'darkslateblue', 'blueviolet', 'darkorchid', 'magenta', 'slateblue', 'mediumslateblue', 'mediumorchid', 'mediumpurple', 'orchid', 'violet', 'plum', 'thistle', 'lavender'),
        `white` = c('mistyrose', 'antiquewhite', 'linen', 'beige', 'whitesmoke', 'lavenderblush', 'oldlace', 'aliceblue', 'seashell', 'ghostwhite', 'honeydew', 'floralwhite', 'azure', 'mintcream', 'snow', 'ivory', 'white'),
        `gray` = c('black', 'darkslategray', 'dimgray', 'slategray', 'gray', 'lightslategray', 'darkgray', 'lightgray', 'gainsboro'))


#' @title
#' Randomly Select Colors By Color Group
#' @param x Vector to map.
#' @param color_group_assignment Vector named by value in X and
#' values as one of c('pink', 'red', 'orange', 'yellow', 'brown', 'green',
#' 'cyan', 'blue', 'purple', 'white', 'gray').
#' @seealso
#'  \code{\link[dplyr]{filter}}
#' @rdname assign_colors
#' @export
#' @importFrom dplyr filter
auto_map_by_color_group <-
        function(x,
                 color_group_assignment,
                 seed = 100) {


                color_assignment <- list()
                for (i in seq_along(color_group_assignment)) {

                        possible_colors <-
                        color_groups[[color_group_assignment[i]]]

                        color_assignment[[i]] <-
                                sample(
                                        x = possible_colors,
                                        size = 1
                                )



                }
                names(color_assignment) <-
                        names(color_group_assignment)

                return(color_assignment)


        }


#' @title
#' Base Colors that Do Not Have Shades
#' @rdname unshaded_base_colors
#' @export
#' @importFrom dplyr select
unshaded_base_colors <-
        function() {
                base_colors()$Unshaded %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }


#' @title
#' Shaded Base Colors
#' @rdname shaded_base_colors
#' @export
#' @importFrom dplyr select
shaded_base_colors <-
        function() {
                base_colors()$Shaded$`4` %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }



#' @title
#' View Colors Via Regex
#' @inheritParams base::grep
#' @inheritParams view_colors
#' @rdname view_colors_regex
#' @export
view_colors_regex <-
        function(pattern,
                 invert = FALSE,
                 labels = TRUE,
                 borders = NULL,
                 cex_label = 0.5,
                 ncol = NULL) {

                colours <-
                        grep(pattern = pattern,
                             x = dplyr::bind_rows(dplyr::bind_rows(base_colors()$Shaded),
                                                  base_colors()$Unshaded) %>%
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


#' @title
#' View Colors Via Regex With Shades
#' @inheritParams base::grep
#' @return OUTPUT_DESCRIPTION
#' @rdname view_color_shades_regex
#' @export
view_color_shades_regex <-
        function(pattern,
                 invert = FALSE) {


                colours <-
                        grep(pattern = pattern,
                             x = dplyr::bind_rows(dplyr::bind_rows(base_colors()$Shaded),
                                                  base_colors()$Unshaded) %>%
                                     select(base_color) %>%
                                     unlist() %>%
                                     unname(),
                             value = TRUE,
                             invert = invert)

                view_color_shades(colours)


        }
