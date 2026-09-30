# Extract Schoenfeld Residuals

This function extracts Schoenfeld residuals from a fitted `cox.zph`
object and formats them into a tidy data frame.

## Usage

``` r
get_schoenfeld(fit_zph)
```

## Arguments

- fit_zph:

  An object of class `cox.zph` produced by the `cox.zph` function,
  representing the Schoenfeld residuals of a Cox proportional hazards
  model.

## Value

A tibble with the Schoenfeld residuals in long format, containing the
columns:

- time:

  The time variable from the Cox model.

- transform:

  The transformation applied to the time variable.

- variable:

  The variable names from the Cox model for which residuals are
  calculated.

- residual:

  The Schoenfeld residuals for each variable at each time point.

## Examples

``` r
library(survival)
test_fit <- survival::coxph(survival::Surv(time, status) ~ sex, data = lung)
test_fit_zph <- survival::cox.zph(test_fit)
get_schoenfeld(test_fit_zph)
#> # A tibble: 165 × 4
#>       time transform variable residual
#>      <dbl> <chr>     <chr>       <dbl>
#>  1 0       km        sex          2.80
#>  2 0.00439 km        sex         -1.81
#>  3 0.00439 km        sex         -1.81
#>  4 0.00439 km        sex         -1.81
#>  5 0.0175  km        sex         -1.82
#>  6 0.0219  km        sex         -1.83
#>  7 0.0219  km        sex         -1.83
#>  8 0.0307  km        sex         -1.84
#>  9 0.0351  km        sex         -1.85
#> 10 0.0395  km        sex         -1.85
#> # ℹ 155 more rows
```
