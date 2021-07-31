#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#' @rdname unshaded_base_colors
#' @export 
#' @importFrom dplyr filter select
unshaded_base_colors <-
        function() {
                base_colors() %>%
                        dplyr::filter(has_shades == FALSE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#' @rdname shaded_base_colors
#' @export 
#' @importFrom dplyr filter select
shaded_base_colors <-
        function() {
                base_colors() %>%
                        dplyr::filter(has_shades == TRUE) %>%
                        dplyr::select(base_color) %>%
                        unlist() %>%
                        unname()
        }



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @param invert PARAM_DESCRIPTION, Default: FALSE
#' @param labels PARAM_DESCRIPTION, Default: TRUE
#' @param borders PARAM_DESCRIPTION, Default: NULL
#' @param cex_label PARAM_DESCRIPTION, Default: 0.5
#' @param ncol PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pattern PARAM_DESCRIPTION
#' @param invert PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname view_color_shades_regex
#' @export 
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
