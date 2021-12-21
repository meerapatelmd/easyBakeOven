#' @title
#' Authorize Google API Access
#' @rdname authorize_google
#' @family Google
#' @export
#' @importFrom gmailr gm_auth_configure gm_auth
authorize_google <-
        function() {

        gmailr::gm_auth_configure(
                key = Sys.getenv("GOOGLE_API_KEY"),
                secret = Sys.getenv("GOOGLE_CLIENT_SECRET")
        )

        gmailr::gm_auth()

}


#' @title
#' Get Gmail Messages By Label
#' @inheritParams gmailr::gm_messages
#' @rdname get_gmail_messages_by_label
#' @family Google
#' @export
#' @importFrom gmailr gm_labels gm_messages
#' @importFrom purrr map transpose pluck
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows select distinct
#' @importFrom secretary typewrite_progress
get_gmail_messages_by_label <-
        function(search = NULL,
                 num_results = 10,
                 include_spam_trash = FALSE) {

        gmail_labels <- gmailr::gm_labels()[[1]]

        names(gmail_labels) <-
                1:length(gmail_labels)

        gmail_labels <-
        gmail_labels %>%
                purrr::map(tibble::as_tibble) %>%
                dplyr::bind_rows() %>%
                dplyr::select(id, name) %>%
                dplyr::distinct()

        gmail_label_list <-
                as.list(gmail_labels$id)
        names(gmail_label_list) <-
                gmail_labels$name


        output <- list()
        for (i in seq_along(gmail_label_list)) {

                secretary::typewrite_progress(
                        iteration = i,
                        total = length(gmail_label_list)
                )

                gmail_label_name <- names(gmail_label_list)[i]
                gmail_label_id <- gmail_label_list[[i]]

                x <-
                gmailr::gm_messages(search = search,
                                    label_ids = gmail_label_id,
                            num_results = num_results,
                            include_spam_trash = include_spam_trash)
                x <- x[[1]]$messages

                if (length(x) > 0) {
                output[[i]] <-
                x %>%
                        purrr::transpose() %>%
                        purrr::pluck("threadId") %>%
                        unlist()

                } else {
                        output[[i]] <- list()
                }

        }


        names(output) <- names(gmail_label_list)
        output
        }


#' @title
#' Get Gmail Labels
#' @rdname get_gmail_labels
#' @family Google
#' @export
#' @import gmailr


get_gmail_labels <-
        function() {
                gmail_labels <- gmailr::gm_labels()[[1]]

                names(gmail_labels) <-
                        1:length(gmail_labels)

                gmail_labels <-
                        gmail_labels %>%
                        purrr::map(tibble::as_tibble) %>%
                        dplyr::bind_rows() %>%
                        dplyr::select(id, name) %>%
                        dplyr::distinct()

                gmail_label_list <-
                        as.list(gmail_labels$id)
                names(gmail_label_list) <-
                        gmail_labels$name

                gmail_label_list

        }



#' @title
#' Get Unread Emails
#' @rdname get_unread_gmail
#' @family Google
#' @export
#' @import gmailr
#' @import tidyverse
#' @import glue

get_unread_gmail <-
        function(days_back = 90,
                 num_results = 1000,
                 include_spam_trash = FALSE) {

                x <-
                gmailr::gm_messages(
                        search = sprintf("after:%s AND is:unread", Sys.Date()-days_back),
                        num_results = num_results,
                        include_spam_trash = include_spam_trash
                )
                x <- x[[1]]$messages
                x <-
                x %>%
                        purrr::transpose() %>%
                        purrr::pluck("threadId") %>%
                        unlist()

                all_labels <-
                        unlist(get_gmail_labels())

                output <- list()
                for (i in seq_along(x)) {
                        thread_id <- x[i]
                        thread_content <- gmailr::gm_thread(thread_id)

                        thread_df <-
                                thread_content$messages[[1]][["payload"]][["headers"]] %>%
                                        purrr::map(as_tibble_row) %>%
                                        dplyr::bind_rows()

                        subject <-
                                thread_df %>%
                                        dplyr::filter(name %in% "Subject") %>%
                                        dplyr::select(value) %>%
                                        unlist() %>%
                                        unname()

                        from <-
                                thread_df %>%
                                dplyr::filter(name %in% "From") %>%
                                dplyr::select(value) %>%
                                unlist() %>%
                                unname()

                        date <-
                                thread_df %>%
                                dplyr::filter(name %in% "Date") %>%
                                dplyr::select(value) %>%
                                unlist() %>%
                                unname()

                        date <-
                        stringr::str_remove_all(string = date,
                                                pattern = "[+-]{1}[0-9]{4}")

                        date <-
                        stringr::str_remove_all(string = date,
                                                pattern = "[(]{1}[A-Z]{3}[)]{1}")

                        date <- trimws(x = date,
                                       which = "both")

                        date <-
                                lubridate::parse_date_time(date,
                                                           orders = c("a, d m Y H:M:S", "d m Y H:M:S"))
                        time_ago <-
                                prettyunits::time_ago(date = date)

                        label_ids      <- unlist(thread_content$messages[[1]]$labelIds)
                        labels_list    <- unlist(all_labels[all_labels %in% label_ids])

                        custom_labels <-
                                names(
                                grep(pattern = "Label_[0-9]{1,}$",
                                     x = labels_list,
                                     value = TRUE))

                        if (length(custom_labels)==0) {
                                custom_labels <- ""
                        }
                        gmail_labels <-
                                names(
                                        grep(pattern = "Label_[0-9]{1,}$",
                                             x = labels_list,
                                             invert = TRUE,
                                             value = TRUE))
                        gmail_labels <- glue::glue_collapse(x = gmail_labels,
                                                      sep = ", ")

                        output[[i]] <-
                                        tibble::tibble(
                                                from = from,
                                                time_ago = time_ago,
                                                subject = subject,
                                                gmail_labels = gmail_labels,
                                                custom_labels = custom_labels
                                        )


                }


                bind_rows(output)


        }

#' @title
#' Get Unread Unlabelled Emails
#' @rdname get_unread_unlabelled_emails
#' @family Google
#' @export
#' @import gmailr
#' @import tidyverse
#' @import glue

get_unread_unlabelled_emails <-
        function() {

                unread_emails <-
                        get_unread_gmail()

                unread_emails %>%
                        dplyr::filter(custom_labels == "")

        }


#' @title
#' Get Inbox
#' @rdname get_gmail_inbox
#' @family Google
#' @export
#' @import gmailr
#' @import tidyverse
#' @import glue
#' @import prettyunits


get_gmail_inbox <-
        function(num_results = 1000) {

                x <-
                        gmailr::gm_messages(
                                num_results = num_results,
                                label_ids = "INBOX"
                        )
                x <- x[[1]]$messages
                x <-
                        x %>%
                        purrr::transpose() %>%
                        purrr::pluck("threadId") %>%
                        unlist()

                all_labels <-
                        unlist(get_gmail_labels())

                output <- list()
                for (i in seq_along(x)) {
                        thread_id <- x[i]
                        thread_content <- gmailr::gm_thread(thread_id)

                        thread_df <-
                                thread_content$messages[[1]][["payload"]][["headers"]] %>%
                                purrr::map(as_tibble_row) %>%
                                dplyr::bind_rows()

                        subject <-
                                thread_df %>%
                                dplyr::filter(name %in% "Subject") %>%
                                dplyr::select(value) %>%
                                unlist() %>%
                                unname()

                        from <-
                                thread_df %>%
                                dplyr::filter(name %in% "From") %>%
                                dplyr::select(value) %>%
                                unlist() %>%
                                unname()

                        date <-
                                thread_df %>%
                                dplyr::filter(name %in% "Date") %>%
                                dplyr::select(value) %>%
                                unlist() %>%
                                unname()

                        date <-
                                stringr::str_remove_all(string = date,
                                                        pattern = "[+-]{1}[0-9]{4}")

                        date <-
                                stringr::str_remove_all(string = date,
                                                        pattern = "[(]{1}[A-Z]{3}[)]{1}")

                        date <- trimws(x = date,
                                       which = "both")

                        date <-
                                lubridate::parse_date_time(date,
                                                           orders = c("a, d m Y H:M:S", "d m Y H:M:S"))
                        time_ago <-
                                prettyunits::time_ago(date = date)

                        label_ids      <- unlist(thread_content$messages[[1]]$labelIds)
                        labels_list    <- unlist(all_labels[all_labels %in% label_ids])

                        custom_labels <-
                                names(
                                        grep(pattern = "Label_[0-9]{1,}$",
                                             x = labels_list,
                                             value = TRUE))

                        if (length(custom_labels)==0) {
                                custom_labels <- ""
                        }
                        gmail_labels <-
                                names(
                                        grep(pattern = "Label_[0-9]{1,}$",
                                             x = labels_list,
                                             invert = TRUE,
                                             value = TRUE))
                        gmail_labels <- glue::glue_collapse(x = gmail_labels,
                                                            sep = ", ")

                        output[[i]] <-
                                tibble::tibble(
                                        from = from,
                                        time_ago = time_ago,
                                        subject = subject,
                                        gmail_labels = gmail_labels,
                                        custom_labels = custom_labels
                                )


                }


                bind_rows(output)


        }
