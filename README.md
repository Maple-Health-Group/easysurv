
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

<div id="install" class="chunk">

<div class="rcode">

``` r
# You will need to have the pak package installed.
install.packages("pak")

# Then, install easysurv with the following line of code.
pak::pkg_install("Maple-Health-Group/easysurv")
```

</div>

</div>

## Getting started

<div id="getting-started" class="chunk">

<div class="rcode">

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

</div>

</div>

## Examples

### Start by tidying your data…

<div id="tidy-data" class="chunk">

<div class="rcode">

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

</div>

</div>

### … then enjoy the easysurv functions!

### `inspect_surv_data()`

``` r
inspect_surv_data(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)
```

<pre class="r-output"><code>#> <span style='color: #555555;'># A tibble: 6 × 12</span>
#>    inst  time status   age   sex ph.ecog ph.karno pat.karno meal.cal wt.loss
#>   <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>  <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>   <span style='color: #555555; font-style: italic;'><dbl></span>    <span style='color: #555555; font-style: italic;'><dbl></span>     <span style='color: #555555; font-style: italic;'><dbl></span>    <span style='color: #555555; font-style: italic;'><dbl></span>   <span style='color: #555555; font-style: italic;'><dbl></span>
#> <span style='color: #555555;'>1</span>     3 10.1       2    74     1       1       90       100     <span style='text-decoration: underline;'>1</span>175      <span style='color: #BB0000;'>NA</span>
#> <span style='color: #555555;'>2</span>     3 14.9       2    68     1       0       90        90     <span style='text-decoration: underline;'>1</span>225      15
#> <span style='color: #555555;'>3</span>     3 33.2       1    56     1       0       90        90       <span style='color: #BB0000;'>NA</span>      15
#> <span style='color: #555555;'>4</span>     5  6.90      2    57     1       1       90        60     <span style='text-decoration: underline;'>1</span>150      11
#> <span style='color: #555555;'>5</span>     1 29.0       2    60     1       0      100        90       <span style='color: #BB0000;'>NA</span>       0
#> <span style='color: #555555;'>6</span>    12 33.6       1    74     1       1       50        80      513       0
#>   event group
#>   <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><fct></span>
#> <span style='color: #555555;'>1</span>     1 Male 
#> <span style='color: #555555;'>2</span>     1 Male 
#> <span style='color: #555555;'>3</span>     0 Male 
#> <span style='color: #555555;'>4</span>     1 Male 
#> <span style='color: #555555;'>5</span>     1 Male 
#> <span style='color: #555555;'>6</span>     0 Male 
#> <span style='color: #555555;'># A tibble: 2 × 2</span>
#>   group      n
#>   <span style='color: #555555; font-style: italic;'><fct></span>  <span style='color: #555555; font-style: italic;'><int></span>
#> <span style='color: #555555;'>1</span> Male     138
#> <span style='color: #555555;'>2</span> Female    90
#> <span style='color: #555555;'># A tibble: 4 × 4</span>
#>   group  event     n percent
#>   <span style='color: #555555; font-style: italic;'><fct></span>  <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><int></span>   <span style='color: #555555; font-style: italic;'><dbl></span>
#> <span style='color: #555555;'>1</span> Male       0    26   0.188
#> <span style='color: #555555;'>2</span> Male       1   112   0.812
#> <span style='color: #555555;'>3</span> Female     0    37   0.411
#> <span style='color: #555555;'>4</span> Female     1    53   0.589
#> <span style='color: #555555;'># A tibble: 2 × 9</span>
#>   records n.max n.start events rmean `se(rmean)` median `0.95LCL` `0.95UCL`
#>     <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>   <span style='color: #555555; font-style: italic;'><dbl></span>  <span style='color: #555555; font-style: italic;'><dbl></span> <span style='color: #555555; font-style: italic;'><dbl></span>       <span style='color: #555555; font-style: italic;'><dbl></span>  <span style='color: #555555; font-style: italic;'><dbl></span>     <span style='color: #555555; font-style: italic;'><dbl></span>     <span style='color: #555555; font-style: italic;'><dbl></span>
#> <span style='color: #555555;'>1</span>     138   138     138    112  10.7       0.753   8.87      6.97      10.2
#> <span style='color: #555555;'>2</span>      90    90      90     53  15.1       1.14   14.0      11.4       18.1
</code></pre>

### `get_km()`

Note that GitHub README files strip most of the colour and formatting
from the console output.

To see the fully formatted output of the following functions, run the
code in your R console.

``` r
km_check <- get_km(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

print(km_check)
```

![](man/figures/get-KM-1.png)<!-- -->
<pre class="r-output"><code>#> 
#> <span style='color: #00BBBB;'>──</span> <span style='font-weight: bold;'>Kaplan-Meier Data</span> <span style='color: #00BBBB;'>───────────────────────────────────────────────────────────</span>
#> The get_km function has produced the following outputs:
#> • <span style='font-weight: bold;'>km</span>: A `survival::survfit()` object for Kaplan-Meier estimates.
#> • <span style='font-weight: bold;'>km_for_excel</span>: A list of stepped Kaplan-Meier data for external plotting.
#> • <span style='font-weight: bold;'>km_per_group</span>: A list of Kaplan-Meier estimates for each group.
#> • <span style='font-weight: bold;'>km_plot</span>: A Kaplan-Meier plot.
#> • <span style='font-weight: bold;'>km_summary</span>: A summary table of the Kaplan-Meier estimates.
#> 
#> ── <span style='font-weight: bold;'>km Summary</span> ──
#> 
#>         group records events    rmean se(rmean)    median   0.95LCL  0.95UCL
#> Male     Male     138    112 10.71324 0.7527413  8.870637  6.965092 10.18480
#> Female Female      90     53 15.13420 1.1397075 13.995893 11.433265 18.06982
#>        Median follow-up
#> Male           27.59754
#> Female         17.37988
#> ────────────────────────────────────────────────────────────────────────────────
#> km_plot has been printed.
#> → For more information, run `View()` on saved get_km output.
</code></pre>

### `test_ph()`

``` r
ph_check <- test_ph(
  data = surv_data,
  time = "time",
  event = "event",
  group = "group"
)

print(ph_check)
```

![](man/figures/test-PH-1.png)<!-- -->![](man/figures/test-PH-2.png)<!-- -->
<pre class="r-output"><code>#> 
#> <span style='color: #00BBBB;'>──</span> <span style='font-weight: bold;'>Proportional Hazards Assumption Testing</span> <span style='color: #00BBBB;'>─────────────────────────────────────</span>
#> 
#> ── <span style='font-weight: bold;'>Cox Proportional Hazards Model</span> ──
#> 
#> `survival::coxph()` output:
#> 
#>                   coef exp(coef)  se(coef)         z    Pr(>|z|)
#> groupFemale -0.5310235 0.5880028 0.1671786 -3.176385 0.001491229
#> 
#> The exp(coef) column shows the hazard ratio was <span style='color: #0000BB;'>0.588</span>.
#> 
#> ── <span style='font-weight: bold;'>Test Survival Curve Differences</span> ──
#> 
#> `survival::survdiff()` found a p-value of <span style='color: #0000BB;'>0.001</span>
#> <span style='color: #00BB00;'>✔</span> suggests survival differences between groups are statistically significant.
#> 
#> ── <span style='font-weight: bold;'>Test the Proportional Hazards Assumption of a Cox Regression</span> ──
#> 
#> `survival::cox.zph()` found a p-value of <span style='color: #0000BB;'>0.091</span>
#> <span style='color: #00BB00;'>✔</span> suggests the PH assumption <span style='font-weight: bold;'>may be</span> valid.
#> 
#>                 p
#> group  0.09062506
#> GLOBAL 0.09062506
#> 
#> ── <span style='font-weight: bold;'>Plots</span> ──
#> 
#> Schoenfeld residuals and log cumulative hazard plots have been printed.
#> ────────────────────────────────────────────────────────────────────────────────
#> <span style='color: #00BBBB;'>ℹ</span> PH tests may not always agree, so consider the results of all tests and plots.
#> <span style='color: #00BBBB;'>ℹ</span> Run `View()` on saved test_ph output to see more.
</code></pre>

### `fit_models()`

``` r
separate_models <- fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "group"
)

print(separate_models)
```

<pre class="r-output"><code>#> 
#> <span style='color: #00BBBB;'>──</span> <span style='font-weight: bold;'>Fit Models Summary</span> <span style='color: #00BBBB;'>──────────────────────────────────────────────────────────</span>
#> <span style='font-weight: bold;'>Engine:</span> <span style='color: #00BB00;'>flexsurv</span>.
#> <span style='font-weight: bold;'>Approach:</span> <span style='color: #00BB00;'>predict_by_other</span>.
#> • The <span style='color: #00BB00;'>predict_by</span> argument was set to <span style='color: #0000BB;'>"group"</span>, which was not a <span style='color: #00BB00;'>covariate</span>.
#> • Therefore, models were fit for each level of <span style='color: #0000BB;'>"group"</span>.
#> • This is sometimes referred to as <span style='color: #0000BB;'>"separate fits"</span>.
#> 
#> <span style='font-weight: bold;'>Distributions attempted:</span> <span style='color: #0000BB;'>"exp"</span>, <span style='color: #0000BB;'>"gamma"</span>, <span style='color: #0000BB;'>"gengamma"</span>, <span style='color: #0000BB;'>"gompertz"</span>, <span style='color: #0000BB;'>"llogis"</span>,
#> <span style='color: #0000BB;'>"lnorm"</span>, and <span style='color: #0000BB;'>"weibull"</span>.
#> 
#> ── <span style='font-weight: bold;'>Median survival estimates</span> ──
#> 
#> ── Group: <span style='color: #0000BB;'>"Male"</span>
#>       dist aic_rank median_est
#> 1      exp        5   7.947301
#> 2    gamma        2   8.505316
#> 3 gengamma        3   8.713519
#> 4 gompertz        4   8.797670
#> 5   llogis        6   8.213242
#> 6    lnorm        7   7.663142
#> 7  weibull        1   8.693753
#> <span style='color: #00BBBB;'>ℹ</span> For comparison, the KM median survival time was <span style='color: #0000BB;'>8.871</span>.
#> <span style='color: #00BBBB;'>ℹ</span> The distribution with the best (lowest) AIC was <span style='color: #0000BB;'>"weibull"</span>.
#> 
#> ── Group: <span style='color: #0000BB;'>"Female"</span>
#>       dist aic_rank median_est
#> 1      exp        6   13.10811
#> 2    gamma        3   13.28173
#> 3 gengamma        4   13.83206
#> 4 gompertz        2   14.33790
#> 5   llogis        5   13.16945
#> 6    lnorm        7   13.09486
#> 7  weibull        1   13.54648
#> <span style='color: #00BBBB;'>ℹ</span> For comparison, the KM median survival time was <span style='color: #0000BB;'>13.996</span>.
#> <span style='color: #00BBBB;'>ℹ</span> The distribution with the best (lowest) AIC was <span style='color: #0000BB;'>"weibull"</span>.
#> ────────────────────────────────────────────────────────────────────────────────
#> → For more information, run `View()` on the fit_models output.
</code></pre>

### `predict_and_plot()`

``` r
plots <- predict_and_plot(
  fit_models = separate_models,
  data = surv_data
)

print(plots)
```

![](man/figures/plot-models-1.png)<!-- -->![](man/figures/plot-models-2.png)<!-- -->![](man/figures/plot-models-3.png)<!-- -->![](man/figures/plot-models-4.png)<!-- -->
<pre class="r-output"><code>#> <span style='color: #00BBBB;'>ℹ</span> Survival and hazard plots have been printed.
</code></pre>
