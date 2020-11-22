# easyBakeOven  
Package helps package authoring by printing back function, yaml, and roxygen2 skeletons for easy customization.  

## Installation  
```  
library(devtools)  
install_github("meerapatelmd/easyBakeOven")  
```  

## Use Cases  
```
new_function <- 
        function(word1,
                 word2,
                 word3) {
                 
                 print(sprintf("%s %s %s", word1, word2, word3))
                 
                 }
                 
                 
library(easyBakeOven)
makeArgs(new_function)

```

## Code of Conduct

  Please note that the suzyBakeOven project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
