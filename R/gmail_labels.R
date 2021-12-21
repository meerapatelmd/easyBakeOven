#' @title
#' Authorize Google API Access
#' @rdname authorize_google
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
#' Get Gmail Lables
#' @rdname get_gmail_labels
#' @export
#' @importFrom gmailr gm_labels gm_messages
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows select distinct
#' @importFrom secretary typewrite_progress
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


        output <- list()
        for (i in seq_along(gmail_label_list)) {

                secretary::typewrite_progress(
                        iteration = i,
                        total = length(gmail_label_list)
                )

                gmail_label_name <- names(gmail_label_list)[i]
                gmail_label_id <- gmail_label_list[[i]]

                output[[i]] <-
                gmailr::gm_messages(label_ids = gmail_label_id,
                            num_results = 100,
                            include_spam_trash = FALSE)

}

        names(output) <- names(gmail_label_list)
        output
}
