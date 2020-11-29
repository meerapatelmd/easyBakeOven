#' @title
#' Create Medallion
#' @importFrom hexSticker sticker
#' @inheritParams hexSticker::sticker
#' @rdname createMedallion
#' @export

createMedallion <-
        function(sourceImg,
                 package,
                 white_around_sticker = TRUE,
                 s_x = 0.8,
                 s_y = 0.75,
                 s_width = 0.4,
                 s_height = 0.5,
                 p_x = 1,
                 p_y = 1.4,
                 p_color = "#FFFFFF",
                 p_family = "Aller_Rg",
                 p_size = 8,
                 h_size = 1.2,
                 h_fill = "#1881C2",
                 h_color = "#87B13F",
                 spotlight = FALSE,
                 l_x = 1,
                 l_y = 0.5,
                 l_width = 3,
                 l_height = 3,
                 l_alpha = 0.4,
                 url = "",
                 u_x = 1,
                 u_y = 0.08,
                 u_color = "black",
                 u_family = "Aller_Rg",
                 u_size = 1.5,
                 u_angle = 30,
                 filename = paste0(package, ".png"),
                 asp = 1,
                 dpi = 300) {
                hexSticker::sticker(subplot = sourceImg,
                                    package = package,
                                    white_around_sticker = white_around_sticker,
                                    s_x = s_x,
                                    s_y = s_y,
                                    s_width = s_width,
                                    s_height = s_height,
                                    p_x = p_x,
                                    p_y = p_y,
                                    p_color = p_color,
                                    p_family = p_family,
                                    p_size = p_size,
                                    h_size = h_size,
                                    h_fill = h_fill,
                                    h_color = h_color,
                                    spotlight = spotlight,
                                    l_x = l_x,
                                    l_y = l_y,
                                    l_width = l_width,
                                    l_height = l_height,
                                    l_alpha = l_alpha,
                                    url = url,
                                    u_x = u_x,
                                    u_y = u_y,
                                    u_color = u_color,
                                    u_family = u_family,
                                    u_size = u_size,
                                    u_angle = u_angle,
                                    filename = filename,
                                    asp = asp,
                                    dpi = dpi
                )
        }

#' @title
#' Create and Move Favicons
#' @description
#' This package first runs the build_favicons() function in the pkgdown package, which auto-detects a logo.svg or logo.png file and runs it through an API. The function makes a copy of the favicon output to the "man/figures" directory for use.
#' @importFrom pkgdown build_favicons
#' @rdname createFavicons
#' @export

createFavicons <-
        function(overwrite = FALSE) {
                pkgdown::build_favicons(overwrite = overwrite)
                cave::dir.create_path(file.path(getwd(), "man/figures"))

                filesToCopy <- list.files("pkgdown/favicon", full.names = TRUE)
                newFiles <- paste0("man/figures/", basename(filesToCopy))
                invisible(
                        mapply(file.copy,
                               from = filesToCopy,
                               to = newFiles
                        )
                )

                cli::cat_bullet('# {PACKAGE_NAME} <img src="man/figures/logo.png" align="right" alt="" width="120" />')
        }
