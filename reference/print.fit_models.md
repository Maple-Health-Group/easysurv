# Print methods for `fit_models()`

Print methods for
[`fit_models()`](https://maple-health-group.github.io/easysurv/reference/fit_models.md)

## Usage

``` r
# S3 method for class 'fit_models'
print(x, ...)
```

## Arguments

- x:

  An object of class `fit_models`

- ...:

  Additional arguments

## Value

A print summary of the `fit_models` object.

## Examples

``` r
models <- fit_models(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  predict_by = "group",
  covariates = "group"
)

models
#> 
#> ── Fit Models Summary ──────────────────────────────────────────────────────────
#> Engine: flexsurv.
#> Approach: predict_by_covariate.
#> • The predict_by argument was set to "group", which was also a covariate.
#> • Therefore, models were fit on the full dataset.
#> • This is sometimes referred to as "joint fits".
#> 
#> Distributions attempted: "exp", "gamma", "gengamma", "gompertz", "llogis",
#> "lnorm", and "weibull".
#> 
#> ── Median survival estimates ──
#> 
#>       dist aic_rank group=Good group=Medium group=Poor
#> 1      exp        7  11.479024     5.065736   2.466988
#> 2    gamma        4   8.511336     4.663958   2.538296
#> 3 gengamma        1   8.630495     4.507538   2.392662
#> 4 gompertz        6   8.530095     4.865718   2.606950
#> 5   llogis        3   8.474352     4.545240   2.336781
#> 6    lnorm        2   8.632037     4.558062   2.393145
#> 7  weibull        5   8.757940     4.741585   2.605819
#> 
#> ℹ For comparison, the KM median survival times were NA, 5.255, and 2.184.
#> ℹ The distribution with the best (lowest) AIC was "gengamma".
#> ────────────────────────────────────────────────────────────────────────────────
#> → For more information, run `View()` on saved `fit_models()` output.
```
