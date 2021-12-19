
inst_dict_file <- "inst/password/dictionary.txt"

if (!file.exists(inst_dict_file)) {
download.file(url = "https://www.gutenberg.org/files/29765/29765-8.txt", destfile = inst_dict_file)
}


inst_dict_terms_file <- "inst/password/dictionary_terms.txt"

if (!file.exists(inst_dict_terms_file)) {

        input <- readLines(inst_dict_file)
        output <-
        grep(pattern = "^[A-Z]{5,}",
             x = input,
             value = TRUE)

        output <-
                grep(pattern = "[[:punct:]]",
                     x = output,
                     invert = TRUE,
                     value = TRUE)

        cat(output,
            file = inst_dict_terms_file,
            sep = "\n",
            append = FALSE)

}



input <- readLines(inst_dict_file)

output_index_a <-
        grep(pattern = "^[A-Z]{5,}",
             x = input,
             value = FALSE)

output_index_b <-
        grep(pattern = "[[:punct:]]",
             x = input,
             invert = FALSE,
             value = FALSE)

output_index <-
        output_index_a[!(output_index_a %in% output_index_b)]
next_line_index <- output_index+1

dict_terms <- input[output_index]
dict_term_next_line <- input[next_line_index]

classify_type <-
        function(x) {

                if (grepl(pattern = "n[.]{1} |n[.]{1}$",
                          x = x)) {

                        return("Noun")
                } else

                if (grepl(pattern = "a[.]{1} |a[.]{1}$",
                          x = x)) {

                        return("Adjective")
                } else

                if (grepl(pattern = "v[.]{1} |v[.]{1}$",
                          x = x)) {

                        return("Verb")
                } else {
                        return("Other")
                }


        }

output <-
lapply(dict_term_next_line,
       classify_type)  %>%
        set_names(dict_terms)
dict_term_type <-
        str_replace_all(string = dict_term_next_line,
                        pattern = "(^.*?[,]{1} )([n. |a. |v. |adv. ])(.*$)",
                        replacement = "\\2")
