library(tidyverse)

# Adds after each function declaration
add_deprecated_call("sample_function1 <-\nfunction(a,b,c){}\n\n#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}")

# Does not skip function declarations that already makes a call to the .Deprecated function
add_deprecated_call("sample_function1 <-\nfunction(a,b,c){\n.Deprecated()\n}\n\n#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}")

# Text can be a vector of length greater than 1
add_deprecated_call(c("sample_function1 <-\nfunction(a,b,c){\n.Deprecated()\n}", "#' Roxygen2 Documentation\n#' Roxygen2 Lines\n\nsample_function2 <- \nfunction(x,y,z){}"))
