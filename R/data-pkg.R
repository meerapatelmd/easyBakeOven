#' @title
#' Data Package Helpers
#'
#' @description
#' Data Package Helpers are functions that support the maintenance of the data that is exported in packages.
#'
#' @param path   Path to a local clone of the target repo.
#' @param conn          Connection object.
#' @name data_package_helpers
NULL

#' @title
#' Document Package Data
#'
#' @description
#' For R packages containing data in the `data-raw` dir, the `usethis.R` is written in the same directory, the data is documented in the `R/data.R`, and `devtools::document` is run on the contents of the provided path.
#'
#' @rdname document_data
#' @export
#' @importFrom devtools document


document_data <-
  function(path) {
    write_usethis_file(path = path)
    write_data_file(path = path)
    devtools::document(pkg = path)
  }




#' @title
#' Create Repo Directories
#'
#' @description
#' Creates the `data-raw`, `data`, and `R` directories within the `path` if they do not already exist.
#'
#' @importFrom cave dir.create_path
#' @export
#' @rdname prep_data_repo_dirs

prep_data_repo_dirs <-
  function(path) {

    # path <- "~/GitHub/Public-Packages/cancergovData/"

    subdirs <- c("data-raw", "data", "R")
    all_paths <- as.list(path.expand(file.path(path, subdirs)))
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

    Files <- path.expand(file.path(path, sprintf("%s.csv", Tables)))

    for (i in seq_along(Tables)) {
      Data <- pg13::read_table(conn = conn,
                               schema = schema,
                               table = Tables[i])
      readr::write_csv(
        x = Data,
        file = Files[i],
        na = na,
        append = append,
        col_names = col_names,
        quote_escape = quote_escape,
        eol = eol
      )
    }

    invisible(Files)
  }

#' @title
#' Write Processing File
#'
#' @rdname write_usethis_file
#' @export

write_usethis_file <-
  function(path) {
    current_wd <- getwd()
    setwd(path)
    on.exit(setwd(current_wd))

    data_raw_path <- file.path(path, "data-raw")

    Files <- list.files(
      path = data_raw_path,
      pattern = "[.]csv$",
      full.names = TRUE
    )

    ObjNames <- stringr::str_remove_all(basename(Files), "[.]{1}[a-zA-Z]{1,}$")

    Lines <- c("library(readr)")

    for (i in seq_along(ObjNames)) {
      Lines <-
        c(
          Lines,
          sprintf('%s <- readr::read_csv("%s")', ObjNames[i], Files[i])
        )
    }


    Lines <-
      c(
        Lines,
        "usethis::use_data(",
        sprintf("\t%s,", ObjNames),
        "overwrite = TRUE",
        ")"
      )


    file <- file.path(data_raw_path, "usethis.R")
    cat(Lines,
      sep = "\n",
      file = file
    )



    source(
      file = file,
      local = TRUE
    )
  }


#' @title
#' Write Processing File
#'
#' @rdname write_data_file
#' @export
#' @importFrom stringr str_replace str_remove_all
#' @importFrom readr read_csv
#' @importFrom sinew makeOxygen

write_data_file <-
  function(path) {
    r_dir_path <- path.expand(file.path(path, "R"))
    r_file_path <- file.path(r_dir_path, "data.R")

    cat(file = r_file_path)

    data_raw_path <- file.path(path, "data-raw")
    Files <- list.files(
      path = data_raw_path,
      pattern = "[.]csv$",
      full.names = TRUE
    )
    ObjNames <- stringr::str_remove_all(basename(Files), "[.]{1}[a-zA-Z]{1,}$")


    for (i in seq_along(ObjNames)) {
      data <- readr::read_csv(Files[i])
      nm <- ObjNames[i]

      doc <- sinew::makeOxygen(
        obj = data,
        print = FALSE
      )
      doc <- stringr::str_replace(
        string = doc,
        pattern = "(.*\")(.*)(\")",
        replacement = paste0("\\1", nm, "\\3")
      )
      doc <- stringr::str_replace(
        string = doc,
        pattern = "DATASET_TITLE",
        replacement = nm
      )

      cat(doc,
        "\n\n",
        file = r_file_path,
        append = TRUE
      )
    }
  }
