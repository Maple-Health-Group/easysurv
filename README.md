
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <b>easysurv</b> <a href="https://maple-health-group.github.io/easysurv/"><img src="man/figures/logo.png" align="right" height="139" alt="easysurv website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Maple-Health-Group/easysurv/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/Maple-Health-Group/easysurv/actions/workflows/check-standard.yaml)
[![test-coverage](https://github.com/Maple-Health-Group/easysurv/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Maple-Health-Group/easysurv/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The *easysurv* R package provides tools to simplify survival data
analysis and model fitting.

*easysurv* facilitates plotting Kaplan-Meier curves, assessing the
proportional hazards assumption, estimating parametric survival models
using engines such as `flexsurv`, `flexsurvspline`, `flexsurvcure` and
`survival`, and exporting associated analyses to Excel.

By default, the package uses the `flexsurv` engine and provides a
helpful starting point to explore survival extrapolations across
frequently used distributions (such as exponential, generalized gamma,
gamma, Gompertz, log-logistic, log-normal and Weibull).

## Installation

If you haven’t already, install [R](https://www.r-project.org) and
consider using [RStudio](https://posit.co/download/rstudio-desktop/) as
your integrated development environment (IDE).

``` r
# You will need to have the pak package installed.
install.packages("pak")

# Then, you can install easysurv with the following line of code.
pak::pkg_install("Maple-Health-Group/easysurv")
```

## Getting started

``` r
# Attach the easysurv library
library(easysurv)

# Open an example script
quick_start()
## Note: The default file name is "easysurv_start.R", but you can define your own, e.g.
## quick_start("my_file_name.R")

# Access help files
help(package = "easysurv")
```

## Examples

### Start by tidying your data

``` r
# Load the easy_lung data from the easysurv package
# Recode the "status" variable to create an event indicator (0/1)
surv_data <- easy_lung |>
  dplyr::mutate(
    time = time,
    event = status - 1,
    group = sex
  )

# Make the group variable a factor and assign level labels.
surv_data <- surv_data |>
  dplyr::mutate_at("group", as.factor)
levels(surv_data$group) <- c("Male", "Female")
```

### `get_km()`

``` r
km_check <- get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

km_check
```

<pre class="r-output"><code>
</code></pre>
<pre class="r-output"><code><span style='color: #00BBBB;'>──</span> <span style='font-weight: bold;'>Kaplan-Meier Data</span> <span style='color: #00BBBB;'>───────────────────────────────────────────────────────────</span>
</code></pre>
<pre class="r-output"><code>The get_km function has produced the following outputs:
</code></pre>
<pre class="r-output"><code>• <span style='font-weight: bold;'>km</span>: A `survival::survfit()` object for Kaplan-Meier estimates.
</code></pre>
<pre class="r-output"><code>• <span style='font-weight: bold;'>km_for_excel</span>: A list of stepped Kaplan-Meier data for external plotting.
</code></pre>
<pre class="r-output"><code>• <span style='font-weight: bold;'>km_per_group</span>: A list of Kaplan-Meier estimates for each group.
</code></pre>
<pre class="r-output"><code>• <span style='font-weight: bold;'>km_plot</span>: A Kaplan-Meier plot.
</code></pre>
<pre class="r-output"><code>• <span style='font-weight: bold;'>km_summary</span>: A summary table of the Kaplan-Meier estimates.
</code></pre>
<pre class="r-output"><code>
</code></pre>
<pre class="r-output"><code>── <span style='font-weight: bold;'>km Summary</span> ──
</code></pre>
<pre class="r-output"><code>
</code></pre>
<pre class="r-output"><code>        group records events    rmean se(rmean)    median   0.95LCL  0.95UCL
Male     Male     138    112 10.71324 0.7527413  8.870637  6.965092 10.18480
Female Female      90     53 15.13420 1.1397075 13.995893 11.433265 18.06982
       Median follow-up
Male           27.59754
Female         17.37988
</code></pre>
<pre class="r-output"><code>────────────────────────────────────────────────────────────────────────────────
</code></pre>
<pre class="r-output"><code>The km_plot has been printed.
</code></pre>
<pre class="r-output"><code>→ For more information, run `View()` on saved get_km output.
</code></pre>

![](man/figures/get-KM-1.png)<!-- -->
