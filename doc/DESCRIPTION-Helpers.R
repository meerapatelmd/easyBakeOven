## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(easyBakeOven)

## -----------------------------------------------------------------------------
makeDescription(path = "~/GitHub/packages/easyBakeOven/", github_user = "meerapatelmd", repo = "easyBakeOven")

## -----------------------------------------------------------------------------
temp_dir <- tempdir()
cat(file = file.path(temp_dir, "DESCRIPTION"))
makeDescription(path = temp_dir, github_user = "meerapatelmd", repo = "easyBakeOven")  

## -----------------------------------------------------------------------------
unlink(temp_dir, recursive = TRUE)

