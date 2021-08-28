#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param text PARAM_DESCRIPTION
#' @param color PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[glue]{glue}}
#' @rdname cat_html_color
#' @export
#' @importFrom glue glue
cat_html_color <-
        function(text,
                 color) {

                glue::glue('<span style="color: {color};">{text}</span>')


        }
