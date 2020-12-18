#' @title
#' Make Installation Section in README
#'
#' @export

makeReadmeInstallation <-
  function(pkg = basename(getwd())) {
    cat(sprintf('# Installation  \n\n```  \nlibrary(devtools)\ninstall_github("meerapatelmd/%s")\n```  \n\n', pkg))
  }
