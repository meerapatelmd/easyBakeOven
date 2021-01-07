#' @title
#' Make Rnw
#'
#' @description
#' Make the Sweave Rnw file for static PDF vignettes with the same file name.
#' The Rnw file will not be written if it already exists, if the `pdf_file`
#' file argument does not exist in the `vignettes` folder, or if `pdf_file` does
#' not have a '.pdf' extension.
#'
#' @param pdf_file Name of the pdf file in the `vignettes/` directory. The full
#' path is not required.
#' @param vignette_title The title of the vignette that will be indexed.
#' @param package Package that the vignette belongs to. Defaults to the basename
#' of the current working directory.
#'
#'
#'
#'
#' @export
#' @rdname make_vignette_rnw
#' @importFrom glue glue
#' @importFrom stringr str_replace
#' @family make vignette functions


make_vignette_rnw <-
        function(pdf_file,
                 vignette_title,
                 package = basename(getwd())) {

                pdf_file <- basename(pdf_file)

                if (!grepl(pattern = "[.]{1}pdf$",
                           x = pdf_file)) {

                        stop("file isn't a pdf")

                }

                if (!file.exists(file.path("vignettes", pdf_file))) {
                        stop(sprintf("pdf_file '%s' does not exist in the vignettes folder.", pdf_file))
                }



                rnw_file <-
                stringr::str_replace(string = pdf_file,
                                     pattern = "(^.*)[.]{1}(pdf$)",
                                     replacement = "\\1.Rnw")
                rnw_file <- file.path("vignettes", rnw_file)


                if (file.exists(rnw_file)) {
                        stop("'%s' already exists", basename(rnw_file))
                }

                cat(
                glue::glue(
                "\\documentclass{article}
                \\usepackage{pdfpages}
                %\\VignetteIndexEntry{[vignette_title]}
                %\\VignettePackage{[package]}
                %\\VignetteEncoding{UTF-8}

                \\begin{document}
                \\SweaveOpts{concordance=TRUE}
                \\includepdf[[pages=-, fitpaper=true]{[pdf_file]}
                \\end{document}",
                .open = "[",
                .close = "]"
                ),
                file = rnw_file,
                append = FALSE)

        }
