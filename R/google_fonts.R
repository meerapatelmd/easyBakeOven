#' @title
#' Use Google Font
#' @description
#' Adds a Google Font from \url{https://fonts.google.com/} and activate the showtext package. This function is particularly useful for using Google Fonts for \code{\link{[createLogo]createLogo}}.
#'
#' @inheritParams sysfonts::font_add_google
#' @seealso
#'  \code{\link[curl]{handle}}
#'  \code{\link[sysfonts]{font_add_google}}
#'  \code{\link[showtext]{showtext_auto}}
#' @rdname activate_google_font
#' @export
#' @importFrom curl new_handle
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto


activate_google_font <-
  function(name,
           regular.wt = 400,
           bold.wt = 700,
           repo = "http://fonts.gstatic.com/",
           db_cache = TRUE,
           handle = curl::new_handle()) {
    do.call(sysfonts::font_add_google,
      args =
        list(
          name = name,
          regular.wt = regular.wt,
          bold.wt = bold.wt,
          repo = repo,
          db_cache = db_cache,
          handle = handle
        ),
      envir = parent.frame()
    )

    do.call(showtext::showtext_auto,
      args = list(rlang::missing_arg()),
      envir = parent.frame()
    )
  }
