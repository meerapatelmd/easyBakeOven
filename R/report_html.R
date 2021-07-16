#' @title
#' Add Link Referencing Another Report
#' @description
#' HTML link to another report based by
#' title. Note that this function does not
#' check if the link is valid and that the corresponding
#' source file exists.
#'
#' @inheritParams create_report
#' @path Optional. Path to HTML to prefix final link with.
#' @seealso
#'  \code{\link[xfun]{file_ext}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname cat_link_to_report
#' @export
#' @importFrom xfun sans_ext with_ext
#' @importFrom stringr str_replace_all

cat_link_to_report <-
        function(rmd_title,
                 path) {

                rmd_title <-
                        xfun::sans_ext(rmd_title)

                html <-
                        xfun::with_ext(
                                x =
                                        stringr::str_replace_all(string = rmd_title,
                                                                 pattern = "[ ]{1}|[:]{1}",
                                                                 replacement = "-"),
                                ext = "html")

                if (!missing(path)) {

                        link <-
                                paste(rmd_title,
                                      path,
                                      collapse = "/")
                } else {
                        link <- html
                }

                cat_html_link(
                        link = link,
                        label = rmd_title
                )



        }
