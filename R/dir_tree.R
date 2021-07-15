#' @title
#' Print Directory Tree
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.tree]{as.Node}},\code{\link[data.tree]{as.Node.data.frame}},\code{\link[data.tree]{as.Node.dendrogram}},\code{\link[data.tree]{as.Node.list}},\code{\link[data.tree]{as.Node.phylo}},\code{\link[data.tree]{as.Node.rpart}}
#' @rdname print_dir_tree
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom data.tree as.Node
print_dir_tree <-
        function(path,
                 root_name = basename(path)) {

                if (any(grepl("[/]{1}", root_name))) {
                        stop("`root_name` cannot have forward slashes")
                }
                dir_tree <-
                        tibble::tibble(pathString = list.dirs(path = path,
                                                              full.names = F,
                                                              recursive = T)) %>%
                        dplyr::mutate(pathString = glue::glue("{root_name}/{pathString}"))

                data.tree::as.Node(dir_tree)
        }

#' @title
#' Print File Tree
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[glue]{glue}}
#'  \code{\link[data.tree]{as.Node}},\code{\link[data.tree]{as.Node.data.frame}},\code{\link[data.tree]{as.Node.dendrogram}},\code{\link[data.tree]{as.Node.list}},\code{\link[data.tree]{as.Node.phylo}},\code{\link[data.tree]{as.Node.rpart}}
#' @rdname print_file_tree
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom glue glue
#' @importFrom data.tree as.Node
print_file_tree <-
        function(path,
                 root_name = basename(path)) {

                if (any(grepl("[/]{1}", root_name))) {
                        stop("`root_name` cannot have forward slashes")
                }
                dir_tree <-
                        tibble::tibble(pathString = list.files(path = path,
                                                              full.names = F,
                                                              recursive = T)) %>%
                        dplyr::mutate(pathString = glue::glue("{root_name}/{pathString}"))

                data.tree::as.Node(dir_tree)
        }
