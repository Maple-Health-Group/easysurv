
<!-- README.md is generated from README.Rmd. Please edit that file -->

# 🍁 easysurv 🍁

## Overview

The easysurv R package provides tools to estimate and inspect
parametric survival models.

## Installation

If you haven’t already, install [R](https://www.r-project.org) and
consider using [RStudio](https://www.rstudio.com/) as your integrated
development environment (IDE).

``` r
# You will need to have the remotes package installed.
install.packages("remotes")

# Then, you can install easysurv with the following line of code.
remotes::install_github("BKievit1/easysurv", auth_token = "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", build_vignettes = TRUE)
```

<!-- 
When we remove the auth_token, we will update from remotes to pak
pak::pkg_install("BKievit1/easysurv")
-->

## Getting Started

``` r
# Load the easysurv library
library(easysurv)

# Open an example script
quick_start()
## Note: The default file name is "easysurv_start.R", but you can define your own, e.g.
## quick_start("my_file_name.R")

# Access help files
help(package = "easysurv")

# Access a detailed vignette
browseVignettes("easysurv")
```
