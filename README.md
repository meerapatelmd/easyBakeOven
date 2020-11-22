# easyBakeOven  

Package helps package authoring by printing back function, yaml, and roxygen2 skeletons for easy customization.  


## Installation  

```  

library(devtools)  
install_github("meerapatelmd/easyBakeOven")  

```  

## Use Cases  

If you have the following functions:  

```

love <- 
        function(thing1) {
                
                sprintf("I love %s.", thing)
                
        }

like <- 
        function(thing2) {
                
                sprintf("I like %s.", thing)
                
        }

```

Making a `love_and_like()` function:   

```  

makeDefaultArgs(love)  
makeDefaultArgs(like)  

```

Copy and paste console text:  

```  
love_and_like <-
        function(thing1,
                 thing2) {
                 
                 
                 }
```  

Repeat for internal arguments:  

```  

makeInternalArgs(love)  
makeInternalArgs(like)  

```

Copy and paste console text:  

```  
love_and_like <-
        function(thing1,
                 thing2) {
                 
                        c(love(thing1 = thing1),
                          like(thing2 = thing2)) %>%
                          paste(collape = "\n") %>%
                          cat()
                 }
```  


## Code of Conduct

  Please note that the suzyBakeOven project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
