#' @title
#' Make a New Rmd from a Template
#'
#' @description
#' Make a copy of a template Rmd file
#' and replace the variables within curly brackets. If a
#' value for a bracketed variable within the template folder is
#' missing, the file will not be written. All templates are
#' required to have an `issue_key` and `report_title`. If a report of the
#' same issue_key and report_title already exists, it will not be overwritten.
#' Sometimes a template will have child Rmds. See 'create_child_report()'
#' to create template children.
#'
#' @param issue_key     Issue identifier.
#' @param report_title  Name of the new report. It cannot contain any
#' forward slashes.
#' @param template_path Path to the template file to use. It can point to a
#' template Rmd in your local machine.
#' @param project_path  Project path assigned to the new Rmd file.
#' @param github_page_path Path to the GitHub site that will host the html
#' output if the feature is enabled.
#' @param source_code_page_path Path to the GitHub repository source code
#' for the Rmd file.
#' @param require_all_vars Variables to replace are designated as those enclosed
#' in curly brackets in the template Rmd file. Sometimes, the template may have
#' variables to be called upon by the glue function iteratively outside the
#' template itself. This argument controls whether or not all of these variables
#' must be assigned a value or not. Default: TRUE.
#' @param ... (Optional) Additional parameters that should be added in the
#' front matter of the Rmd. Should be entered in the format of a named vector.
#' @return
#' @seealso
#'  \code{\link[xfun]{read_utf8}}
#'  \code{\link[cli]{cli_alert}}
#'  \code{\link[cave]{dir.create_path}}
#' @rdname create_report
#' @export
#' @importFrom xfun read_utf8
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom cave dir.create_path
create_report <-
  function(issue_key,
           report_title,
           project_path = getwd(),
           github_page_path = "",
           source_code_page_path = "",
           template_path = system.file(package = "easyBakeOven", "reports", "Generic.Rmd"),
           require_all_vars = TRUE,
           ...) {

    stopifnot(!missing(report_title))
    stopifnot(!grepl(pattern = "[/]{1}", x = report_title))

    report_tree()

    var_values <-
    rlang::list2(project_path = project_path,
                 issue_key = issue_key,
                 report_title = report_title,
                 github_page_path = github_page_path,
                 source_code_page_path = source_code_page_path,
                 ...)

    new_rmd_file <-
      sprintf("rmd/%s: %s.Rmd", issue_key, report_title)

    if (file.exists(new_rmd_file)) {
      stop(sprintf("`%s` already exists and was not overwritten.",
                  new_rmd_file))
    }

    new_rmd <- xfun::read_utf8(template_path)
    var_names <- vector()
    for (var_num in seq_along(var_values)) {
      var_name    <- names(var_values)[var_num]
      var_pattern <- sprintf("[{]{1}%s[}]{1}", var_name)
      var_value   <- var_values[[var_name]]
      if (any(grepl(pattern = var_pattern, new_rmd))) {

        new_rmd <-
          stringr::str_replace_all(
            string = new_rmd,
            pattern = var_pattern,
            replacement = var_value
          )

        var_names <-
          c(var_names,
            var_name)

      }
    }
    var_values <-
      var_values[!(names(var_values) %in% var_names)]

  # Are there any user-provided variables that need to be added to the parameters?
  if (length(var_values)>0) {
    # Creating the lines to add under params:
    new_params <- vector()
    for (var_num in seq_along(var_values)) {
      new_params <-
        c(new_params,
          sprintf("  %s: %s", names(var_values)[var_num], var_values[[var_num]]))
    }



    params_x <- grep(pattern = "params[:]{1}",
                     x = new_rmd)[1]

    if (length(params_x) == 0) {
      stop("'params:' not found.")
    }
    new_rmd2 <-
      new_rmd[1:params_x]

    new_rmd2 <-
      c(new_rmd2,
        new_params,
        new_rmd[(params_x+1):length(new_rmd)])

    new_rmd <-
      new_rmd2
  }

  # Are there any variables within the template that have not been replaced?
  unrep_vars <-
    grep(pattern = "[^```]{1}[{]{1}.*?[}]",
         new_rmd,
         value = TRUE,
         perl = TRUE)

  # Filtering out counts within {} in regex
  unrep_vars <-
    grep(pattern = "[{]{1}[0-9]{1,}[,]{0,1}[0-9]{0,}[}]{1}",
         unrep_vars,
         invert = TRUE,
         value = TRUE)

  if (length(unrep_vars)>0) {
    unrep_vars <- unlist(strsplit(unrep_vars, split = "[ ]{1}"))
    unrep_vars <- trimws(unrep_vars,
                         which = "both")
    unrep_vars <- grep(pattern = "^[{]{1}.*?[}]{1}$",
                       unrep_vars,
                       value = TRUE)
    unrep_vars <- stringr::str_replace_all(unrep_vars,
                                           pattern = "[{]{1}(.*?)[}]{1}",
                                           replacement = "\\1")
    unrep_vars <- sprintf("`%s`", unrep_vars)
    unrep_vars <- paste(unrep_vars, collapse = ", ")

    if (require_all_vars) {
    stop(sprintf("Missing values for vars: %s.\n", unrep_vars),
         call. = FALSE)
    } else {
      warning(sprintf("Missing values for vars: %s.\n", unrep_vars),
           call. = FALSE)
    }
  }



  cat(new_rmd,
      file = new_rmd_file,
      sep = "\n")

  cli::cli_alert_success(text = new_rmd_file)


  # Adding Data Subdirectories
  data_folders =
    c("raw",
      "intermediate",
      "final",
      "outgoing")

    data_paths <-
      file.path(getwd(),
                "data",
                issue_key,
                report_title,
                data_folders)

  for (data_path in data_paths) {
    if (!dir.exists(data_path)) {
      cave::dir.create_path(data_path)
    }
  }

    # Adding child folder in rmd/
    child_rmd_path <-
      file.path(getwd(),
                "rmd",
                issue_key,
                report_title)

    if (!dir.exists(child_rmd_path)) {cave::dir.create_path(child_rmd_path)}

    # Adding Image Directory
    img_path <-
      file.path(getwd(),
                "img",
                issue_key,
                report_title)

    cave::dir.create_path(img_path)

    if (interactive()) {
      file.edit(new_rmd_file)

    }

  }

#' @title
#' Remove a Report
#' @description
#' Removes all the directories and files based on a issue_key and report_title pair.
#' No changes are made in cases where the provided pair does not exist in the
#' working directory or the function is not run interactively.
#' @inheritParams create_report
#' @seealso
#'  \code{\link[cli]{cat_line}},\code{\link[cli]{cli_alert}},\code{\link[cli]{cli_bullets}}
#'  \code{\link[secretary]{press_enter}}
#' @rdname remove_report
#' @export
#' @importFrom cli cat_rule cli_alert_danger cli_alert_warning cli_bullets cli_alert_info cat_bullet
#' @importFrom secretary press_enter

remove_report <-
  function(issue_key,
           report_title) {

    if (interactive()) {
    cli::cat_rule("Directories")
    dir_pattern <- sprintf("[/]{1}%s/%s$|[/]{1}%s[/]{1}%s[/]{1}", issue_key, report_title, issue_key,
                                 report_title)
    dirs_to_remove <-
      grep(pattern = dir_pattern,
           x = list.dirs(recursive = TRUE),
           value = TRUE)

    if (length(dirs_to_remove)==0) {
      cli::cli_alert_danger(sprintf("Issue '%s' Report '%s' dirs not found. No changes made.", issue_key, report_title))
    } else {
      cli::cli_alert_warning("The following dir/s will be unlinked:")
      names(dirs_to_remove) <- rep(" ", length(dirs_to_remove))
      cli::cli_bullets(dirs_to_remove)
      secretary::press_enter()
      cli::cli_alert_info("Unlinking dirs...")
      for (dir_to_remove in dirs_to_remove) {
        unlink(dir_to_remove,
               recursive = TRUE)
        cli::cat_bullet(dir_to_remove,
                        bullet = "tick",
                        bullet_col = "green")

      }
    }

    rmd_pattern <- sprintf("%s: %s.Rmd$", issue_key, report_title)
    rmds_to_remove <-
    list.files(pattern = rmd_pattern,
               full.names = TRUE,
               recursive = TRUE)

    cli::cat_rule("Rmd")
    if (length(rmds_to_remove)==0) {
      cli::cli_alert_danger(sprintf("Issue '%s' Report '%s' Rmd not found. No changes made.", issue_key, report_title))
    } else {
      cli::cli_alert_warning("The following file/s will be unlinked:")
      names(rmds_to_remove) <- rep(" ", length(rmds_to_remove))
      cli::cli_bullets(rmds_to_remove)
      secretary::press_enter()
      cli::cli_alert_info("Removing files...")
      for (rmd_to_remove in rmds_to_remove) {
        file.remove(rmd_to_remove)
        cli::cat_bullet(rmd_to_remove,
                        bullet = "tick",
                        bullet_col = "green")

      }
    }

    cli::cat_rule("HTML")
    html_pattern <- sprintf("%s--%s.html$", issue_key, report_title)
    html_pattern <-
    stringr::str_replace_all(html_pattern,
                             pattern = "[ ]{1}",
                             replacement = "-")
    htmls_to_remove <-
    list.files(pattern = html_pattern,
               full.names = TRUE,
               recursive = TRUE)
    if (length(htmls_to_remove)==0) {
      cli::cli_alert_danger(sprintf("Issue '%s' Report '%s' html not found. No changes made.", issue_key, report_title))
    } else {
      cli::cli_alert_warning("The following file/s will be unlinked:")
      names(htmls_to_remove) <- rep(" ", length(htmls_to_remove))
      cli::cli_bullets(htmls_to_remove)
      secretary::press_enter()
      cli::cli_alert_info("Removing files...")
      for (html_to_remove in htmls_to_remove) {
        file.remove(html_to_remove)
        cli::cat_bullet(html_to_remove,
                        bullet = "tick",
                        bullet_col = "green")

      }
    }

    }

  }

#' @keywords internal

report_tree <-
  function(path = getwd()) {
    dir_tree <-
      c(
        path.expand(file.path(path, "rmd", "templates")),
        path.expand(file.path(path, "output")))

    sapply(dir_tree,
           create_path)

    file.copy(
      from = system.file(package = "easyBakeOven",
                         "reports",
                         "style.css"),
      to = path.expand(file.path(path, "rmd"))
    )

  }


#' @title
#' Create a Child Rmd from a Template's Child
#' @description
#' Sometimes a template Rmd will also have children.
#' This function makes a copy of the template child at the given `template_path`
#' and copies it to the `child_rmd_folder` with the same filename. Unlike
#' `create_report()`, subdirectories are not created since that is the
#' work of the main template.
#' @inheritParams create_report
#' @param child_rmd_folder Path that the template will be copied to.
#' @seealso
#'  \code{\link[rlang]{list2}}
#'  \code{\link[xfun]{read_utf8}}
#'  \code{\link[stringr]{str_replace}}
#'  \code{\link[cli]{cli_alert}}
#' @rdname create_report_child
#' @export
#' @importFrom rlang list2
#' @importFrom xfun read_utf8
#' @importFrom stringr str_replace_all
#' @importFrom cli cli_alert_success

create_report_child <-
  function(template_path,
           child_rmd_folder, #Destination folder for new template to be copied
           require_all_vars = TRUE,
           ...) {


    var_values <-
      rlang::list2(...)

    new_rmd_file <-
      file.path(child_rmd_folder, basename(template_path))

    if (file.exists(new_rmd_file)) {
      stop(sprintf("`%s` already exists and was not overwritten.",
                   new_rmd_file))
    }

    new_rmd <- xfun::read_utf8(template_path)
    var_names <- vector()
    for (var_num in seq_along(var_values)) {
      var_name    <- names(var_values)[var_num]
      var_pattern <- sprintf("[{]{1}%s[}]{1}", var_name)
      var_value   <- var_values[[var_name]]
      if (any(grepl(pattern = var_pattern, new_rmd))) {

        new_rmd <-
          stringr::str_replace_all(
            string = new_rmd,
            pattern = var_pattern,
            replacement = var_value
          )

        var_names <-
          c(var_names,
            var_name)

      }
    }
    var_values <-
      var_values[!(names(var_values) %in% var_names)]

    # Are there any user-provided variables that need to be added to the parameters?
    if (length(var_values)>0) {
      # Creating the lines to add under params:
      new_params <- vector()
      for (var_num in seq_along(var_values)) {
        new_params <-
          c(new_params,
            sprintf("  %s: %s", names(var_values)[var_num], var_values[[var_num]]))
      }



      params_x <- grep("params[:]{1}",
                       new_rmd)
      new_rmd2 <-
        new_rmd[1:params_x]

      new_rmd2 <-
        c(new_rmd2,
          new_params,
          new_rmd[(params_x+1):length(new_rmd)])

      new_rmd <-
        new_rmd2
    }

    # Are there any variables within the template that have not been replaced?
    unrep_vars <-
      grep(pattern = "[^```]{1}[{]{1}.*?[}]",
           new_rmd,
           value = TRUE,
           perl = TRUE)

    # Filtering out counts within {} in regex
    unrep_vars <-
      grep(pattern = "[{]{1}[0-9]{1,}[,]{0,1}[0-9]{0,}[}]{1}",
           unrep_vars,
           invert = TRUE,
           value = TRUE)

    if (length(unrep_vars)>0) {
      unrep_vars <- unlist(strsplit(unrep_vars, split = "[ ]{1}"))
      unrep_vars <- trimws(unrep_vars,
                           which = "both")
      unrep_vars <- grep(pattern = "^[{]{1}.*?[}]{1}$",
                         unrep_vars,
                         value = TRUE)
      unrep_vars <- stringr::str_replace_all(unrep_vars,
                                             pattern = "[{]{1}(.*?)[}]{1}",
                                             replacement = "\\1")
      unrep_vars <- sprintf("`%s`", unrep_vars)
      unrep_vars <- paste(unrep_vars, collapse = ", ")
      if (require_all_vars) {
      stop(sprintf("Missing values for vars: %s.\n", unrep_vars),
           call. = FALSE)
      } else {
        warning(sprintf("Missing values for vars: %s.\n", unrep_vars),
                call. = FALSE)
      }
    }



    cat(new_rmd,
        file = new_rmd_file,
        sep = "\n")

    cli::cli_alert_success(text = new_rmd_file)



    if (interactive()) {
      file.edit(new_rmd_file)

    }

  }
