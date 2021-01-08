#' @noRd

create_path <-
        function (dir)
        {
                dir <- path.expand(dir)
                all_dirs <- unlist(strsplit(x = dir, split = .Platform$file.sep))
                all_dirs <- no_blank(all_dirs)
                dirs1 <- list()
                for (i in seq_along(all_dirs)) {
                        if (i == 1) {
                                dirs1[[i]] <- all_dirs[i]
                        }
                        else {
                                dirs1[[i]] <- c(dirs1[[i - 1]], all_dirs[i])
                        }
                }
                formatted_dir_paths <- list()
                for (i in seq_along(dirs1)) {
                        if (i == 1) {
                                formatted_dir_paths[[i]] <- sprintf("%s%s", .Platform$file.sep,
                                                                    dirs1[[i]])
                        }
                        else {
                                formatted_dir_paths[[i]] <- do.call(what = file.path,
                                                                    args = as.list(dirs1[[i]]))
                                formatted_dir_paths[[i]] <- sprintf("/%s", formatted_dir_paths[[i]])
                        }
                }
                for (i in seq_along(formatted_dir_paths)) {
                        if (!dir.exists(paths = formatted_dir_paths[[i]])) {
                                dir.create(formatted_dir_paths[[i]])
                                message(sprintf("%s directory created", formatted_dir_paths[[i]]))
                        }
                        else {
                                message(sprintf("%s already exists", formatted_dir_paths[[i]]))
                        }
                }
        }
