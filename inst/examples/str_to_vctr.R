
# String to Vector
test_vctr <- "c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J')"
str_to_vctr(test_vctr)

test_vctr <- 'c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")'
str_to_vctr(test_vctr)

test_vctr <- 'c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\", \"G\", \"H\", \"I\", \"J\")'
str_to_vctr(test_vctr)


# Vector to String
vctr_to_str(LETTERS[1:5])

vctr_to_str(LETTERS[1:5], quote = "\"")
