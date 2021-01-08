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


#' @title
#' Add a Static Vignette with R.rsp
#'
#' @description
#' Make the asis file for static PDF or HTML vignettes which is named the same as
#' the `file` argument with".asis" appended. The asis file will not be written
#' if it already exists, if the `file` file argument does not exist in the
#' `vignettes` folder, or if `file` does not have a '.pdf' or '.html' extension.
#'
#' Compared to \code{\link{make_vignette_rnw}}, make_vignette_asis allows using
#' static HTML vignettes. However, it requires adding R.rsp to the Suggests and
#' VignetteBuilder fields in the DESCRIPTION file to execute.
#'
#' @param file Name of the pdf or html file in the `vignettes/` directory. The full
#' path is not required.
#' @param vignette_title The title of the vignette that will be indexed.
#' @param package Package that the vignette belongs to. Defaults to the basename
#' of the current working directory.
#'
#'
#'
#'
#' @export
#' @rdname make_vignette_asis
#' @importFrom glue glue
#' @importFrom secretary typewrite
#' @importFrom cli cat_bullet
#' @family make vignette functions



make_vignette_asis <-
        function(file,
                 vignette_title,
                 package = basename(getwd())) {

                file <- basename(file)

                if (!grepl(pattern = "[.]{1}pdf$|[.]{1}html$",
                           x = file)) {

                        stop("file isn't a pdf or html")

                }

                if (!file.exists(file.path("vignettes", file))) {
                        stop(sprintf("file '%s' does not exist in the vignettes folder.", file))
                }



                asis_file <- paste0(file, ".asis")
                asis_file <- file.path("vignettes", asis_file)


                if (file.exists(asis_file)) {
                        stop("'%s' already exists", basename(asis_file))
                }

                cat(
                        glue::glue(
                "%\\VignetteIndexEntry{[vignette_title]}
                %\\VignetteEngine{R.rsp::asis}
                %\\VignettePackage{[package]}",
                                .open = "[",
                                .close = "]"
                        ),
                        file = asis_file,
                        append = FALSE)

                secretary::typewrite("The following must be added to DESCRIPTION:",
                                     timepunched = FALSE)
                cli::cat_bullet("Suggests: R.rsp")
                cli::cat_bullet("VignetteBuilder: R.rsp")
        }
