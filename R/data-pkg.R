#' @title
#' Data Package Helpers
#'
#' @description
#' Data Package Helpers are functions that support the maintenance of the data that is exported in packages.
#'
#' @param repo_path   Path to a local clone of the target repo.
#' @param conn          Connection object.
#' @name data_package_helpers
NULL

#' @title
#' Deploy Package Data
#'
#' @rdname deploy_data
#' @export
#' @importFrom devtools document


deploy_data <-
        function(repo_path) {
                execute_usethis(repo_path = repo_path)
                write_data_r_file(repo_path = repo_path)
                devtools::document(pkg = repo_path)
        }




#' @title
#' Create Repo Directories
#'
#' @description
#' Creates the `data-raw`, `data`, and `R` directories within the `repo_path` if they do not already exist.
#'
#' @importFrom cave dir.create_path
#' @export
#' @rdname prep_data_repo_dirs

prep_data_repo_dirs <-
        function(repo_path) {

                # repo_path <- "~/GitHub/Public-Packages/cancergovData/"

                subdirs <- c("data-raw", "data", "R")
                all_paths <- as.list(path.expand(file.path(repo_path, subdirs)))
                names(all_paths) <- c("DATA_RAW", "DATA", "R")

                for (i in seq_along(all_paths)) {

                        cave::dir.create_path(all_paths[[i]])

                }

                invisible(all_paths)

        }



#' @title
#' Export a Schema to CSV Files
#'
#' @rdname write_schema_to_csvs
#' @export
#' @inheritParams readr::write_csv

write_schema_to_csvs <-
        function(conn,
                 schema,
                 path,
                 na = "NA",
                 append = FALSE,
                 col_names = !append,
                 quote_escape = "double",
                 eol = "\n") {

                Tables <- pg13::lsTables(conn = conn, schema = schema)

                Data <-
                        Tables %>%
                                purrr::map(~ pg13::readTable(conn = conn,
                                                             schema = schema,
                                                             tableName = .))

                Files <- path.expand(file.path(path, sprintf("%s.csv", Tables)))

                for (i in seq_along(Data)) {

                        readr::write_csv(x = Data[[i]],
                                         file = Files[i],
                                         na = na,
                                         append = append,
                                         col_names = col_names,
                                         quote_escape = quote_escape,
                                         eol = eol)
                }

                invisible(Files)

        }

#' @title
#' Write Processing File
#'
#' @rdname execute_usethis
#' @export

execute_usethis <-
        function(repo_path) {

                current_wd <- getwd()
                setwd(repo_path)
                on.exit(setwd(current_wd))

                data_raw_path <- file.path(repo_path, "data-raw")

                Files <- list.files(path = data_raw_path,
                                    pattern = "[.]csv$",
                                    full.names = TRUE)

                ObjNames <- stringr::str_remove_all(basename(Files), "[.]{1}[a-zA-Z]{1,}$")

                Lines <- c("library(readr)")

                for (i in seq_along(ObjNames)) {

                        Lines <-
                                c(Lines,
                                  sprintf('%s <- readr::read_csv("%s")', ObjNames[i], Files[i]))

                }


                Lines <-
                        c(Lines,
                          "usethis::use_data(",
                          sprintf("\t%s,", ObjNames),
                          "overwrite = TRUE",
                          ")"
                          )


                file <- file.path(data_raw_path, "usethis.R")
                cat(Lines,
                    sep = "\n",
                    file = file)



                source(file = file,
                       local = TRUE)
        }


#' @title
#' Write Processing File
#'
#' @rdname write_data_r_file
#' @export
#' @importFrom stringr str_replace str_remove_all
#' @importFrom readr read_csv
#' @importFrom sinew makeOxygen

write_data_r_file <-
        function(repo_path) {

                r_dir_path <- path.expand(file.path(repo_path, "R"))
                r_file_path <- file.path(r_dir_path, "data.R")

                cat(file = r_file_path)

                data_raw_path <- file.path(repo_path, "data-raw")
                Files <- list.files(path = data_raw_path,
                                    pattern = "[.]csv$",
                                    full.names = TRUE)
                ObjNames <- stringr::str_remove_all(basename(Files), "[.]{1}[a-zA-Z]{1,}$")


                for (i in seq_along(ObjNames)) {

                        data <- readr::read_csv(Files[i])
                        nm <- ObjNames[i]

                        doc <- sinew::makeOxygen(obj = data,
                                                 print = FALSE)
                        doc <- stringr::str_replace(string = doc,
                                                    pattern = "(.*\")(.*)(\")",
                                                    replacement = paste0("\\1", nm, "\\3"))
                        doc <- stringr::str_replace(string = doc,
                                                    pattern = "DATASET_TITLE",
                                                    replacement = nm)

                        cat(doc,
                            "\n\n",
                            file = r_file_path,
                            append = TRUE)

                }
        }
