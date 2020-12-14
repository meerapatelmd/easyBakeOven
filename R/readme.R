#' @title
#' Make Installation Section in README
#'
#' @export

makeInstallationSection <-
        function(pkg = basename(getwd())) {

                cat(sprintf('# Installation  \n\n```  \nlibrary(devtools)\ninstall_github("meerapatelmd/%s")\n```  \n\n', pkg))
        }
