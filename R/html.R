#' @title
#' Make a HTML link to a new tab
#' @param link  HTML link
#' @param label Label for HTML link
#' @rdname cat_html_link
#' @export

cat_html_link <-
        function(link,
                 label) {

                sprintf('<a target=_blank href="%s">%s</a>',
                        link,
                        label)
        }
