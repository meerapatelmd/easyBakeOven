
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easyBakeOven <img src="man/figures/logo.png" align="right" alt="" width="200" />

<!-- badges: start -->

<!-- badges: end -->

R Package that facilitates various aspects of R package authorship and
deployment in tandem with the usethis, devtools, and sinew packages.
Features include:

  - Generate yaml skeletons for the pkgdown package  
  - Write Rnw files for Sweave to incorporate static pdf vignettes into
    a package  
  - Customize sinew package’s roxygen2 skeletons

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("meerapatelmd/easyBakeOven")
```

## Example

### Printing arguments and their values in the R console

If you have the following functions:

``` r
love <- 
        function(thing_i_love) {
                
                sprintf("I love %s.", thing_i_love)
                
        }

like <- 
        function(thing_i_like) {
                
                sprintf("I like %s.", thing_i_like)
                
        }
```

You want to print what you love or what you like. For example, if you
love apples:

``` r
love("apples")
#> [1] "I love apples."
```

If you like bananas:

``` r
like("bananas")
#> [1] "I like bananas."
```

What if I want to author a new function that prints both `things I love`
and `things I like` in a single function call?

I can use `easyBakeOven`’s `make_args` functions to return arguments and
their values for `love()` and `like()` to copy-and-paste into the new
function.

``` r
library(easyBakeOven)
library(tidyverse)
```

The default and internal arguments from `love()` are:

``` r
make_args(love)
#> thing_i_love
#> 
#> thing_i_love = thing_i_love
```

The same for `like()` are:

``` r
make_args(like)
#> thing_i_like
#> 
#> thing_i_like = thing_i_like
```

I can then copy-and-paste the results from the above calls to
`make_args` to author `like_and_love()`. Starting with the default
values for the arguments for  
both functions, both of which are missing.

``` r
like_and_love <-
        # Added default arguments
        function(thing_i_love,
                 thing_i_like) {
                         
                
        }
```

The internal arguments can then be copy-and-pasted.

``` r
like_and_love <-
        function(thing_i_love,
                 thing_i_like) {
                         
                         sprintf("%s AND %s",
                                 # Added internal arguments
                                 love(thing_i_love = thing_i_love),
                                 like(thing_i_like = thing_i_like))
                
        }
```

The function is now authored and executable.

``` r
like_and_love(thing_i_love = "apples",
              thing_i_like = "bananas")
#> [1] "I love apples. AND I like bananas."
```

## Code of Conduct

Please note that the suzyBakeOven project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
