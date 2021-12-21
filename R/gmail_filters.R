


process_gmail_filters_xml <-
        function(xml_path) {

        gmail_filters_xml <- xml2::read_xml(x = xml_path)
        gmail_filter_data <- XML::xmlParse(gmail_filters_xml)
        gmail_filter_list <- XML::xmlToList(gmail_filter_data)

        gmail_filter_list_entries <-
                gmail_filter_list[
                        grep(pattern = "entry",
                             x = names(gmail_filter_list))]

        names(gmail_filter_list_entries) <-
                1:length(gmail_filter_list_entries)


        gmail_filter_depivoted <-
        gmail_filter_list_entries %>%
                map(function(x)
                        x[grep(pattern = "property",
                               x = names(x))]) %>%
                map(function(x)
                        map(x, function(y) as_tibble_row(y))) %>%
                map(function(x) bind_rows(x)) %>%
                bind_rows(.id = "filter_index") %>%
                dplyr::filter(!(name %in% c("sizeOperator", "sizeUnit")))


        gmail_filter_pivoted <-
        gmail_filter_depivoted %>%
                tidyr::pivot_wider(id_cols = filter_index,
                                   names_from = name,
                                   values_from = value)

        logic_fields <-
                c("subject",
                  "doesNotHaveTheWord",
                  "from",
                  "hasTheWord",
                  "to",
                  "hasAttachment")


        consequence_fields <-
                c(
                        'shouldNeverSpam',
                        'shouldMarkAsRead',
                        'shouldArchive',
                        'shouldTrash',
                        'forwardTo'
                )

        identifier_fields <-
                c("label")

        gmail_filter_pivoted %>%
                dplyr::select(all_of(identifier_fields),
                              all_of(logic_fields),
                              all_of(consequence_fields)) %>%
                dplyr::arrange(label)

}
