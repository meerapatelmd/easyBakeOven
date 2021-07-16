#' @title
#' Cache or Load an Object Depending on
#' Interactive Status
#' @description
#' If called in an interactive session, the object will be cached to be
#' subsequently loaded in a non-interactive session, such as when R markdown
#' is knit. A previous run of a given chunk is required and an error will be
#' thrown if the cached object does not exist.
#'
#' @param object Object to be cached.
#' @param dirs   Required. Path to the subdirectory within R.cache root path to
#' save the cache to.
#' @return       If called in a non-interactive session, the object will be returned
#' in the parent frame without being declared.
#'
#' @details
#' This function is useful when reporting on a process that has already been
#' executed and does not require re-execution during the knitting process.
#' For example, a Neo4j database can be written in an interactive session at
#' which point the metadata for reporting purposes can be logged. Instead of forcing
#' the database to be instantiated when the R markdown is knit, knitr will
#' only pull up the cached log.
#'
#' @seealso
#'  \code{\link[R.cache]{saveCache}},\code{\link[R.cache]{findCache}},\code{\link[R.cache]{loadCache}}
#' @rdname preknit_cache
#' @export
#' @importFrom R.cache saveCache findCache loadCache


preknit_cache <-
        function(object,
                 dirs) {

                stopifnot(!missing(dirs))

                object_name <-
                        deparse(substitute(object))

                if (interactive()) {

                        R.cache::saveCache(
                                object = object,
                                dirs   = dirs,
                                key    = list(object_name)
                        )

                } else {

                        # Checking to see if a cache for this key already exists
                        object_is_cached <-
                        !is.null(
                                R.cache::findCache(
                                        dirs = dirs,
                                        key  = list(object_name)
                                )
                        )


                        if (!object_is_cached) {
                                stop(sprintf("`%s` is not cached at `%s`.", object_name,
                                             cache_folder))
                        }

                        value <-
                        R.cache::loadCache(
                                dirs   = dirs,
                                key    = list(object_name))

                        assign(x = object_name,
                               value = value,
                               envir = parent.frame())

                }

        }
