library(tidyverse)

# Adds "Deprecated" after each Roxygen2 Tag
add_deprecated_to_desc(text = "#' This is the title\n#' @description\n#' This is the description.")
