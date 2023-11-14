
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easysurv

<!-- badges: start -->

[![R-CMD-check](https://github.com/Maple-Health-Group/easysurv/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Maple-Health-Group/easysurv/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/Maple-Health-Group/easysurv/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Maple-Health-Group/easysurv/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

## Overview

The *easysurv* R package provides tools to estimate and inspect
parametric survival models.

The package is built upon the *flexsurv* engine, and aims to provide a
helpful starting point to explore survival extrapolations across
frequently used distributions (such as exponential, generalized gamma,
gamma, Gompertz, log-logistic, log-normal and Weibull).

## Installation

If you haven’t already, install [R](https://www.r-project.org) and
consider using [RStudio](https://www.rstudio.com/) as your integrated
development environment (IDE).

``` r
# You will need to have the pak package installed.
install.packages("pak")

# Then, you can install easysurv with the following line of code.
pak::pkg_install("Maple-Health-Group/easysurv")
```

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
