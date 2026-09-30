# Fit Survival Models

Fits survival models to the provided data using the specified engine and
returns various outputs including model parameters, goodness of fit, and
estimates of median survival.

## Usage

``` r
fit_models(
  data,
  time,
  event,
  predict_by = NULL,
  covariates = NULL,
  dists = c("exp", "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull"),
  engine = "flexsurv",
  k = c(1, 2, 3),
  scale = "hazard",
  add_time_0 = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame containing the survival data.

- time:

  The name of the column in `data` containing the time-to-event
  information.

- event:

  The name of the column in `data` indicating whether the event of
  interest occurred.

- predict_by:

  (Optional) The name of the column in `data` defining the prediction
  variable.

- covariates:

  (Optional) A character vector specifying the names of covariates to be
  included in the model.

- dists:

  (Optional) A character vector specifying the distribution(s) to be
  fitted.

  When the engine parameter is set to "flexsurv", options are "exp",
  "exponential", "gamma", "genf", "genf.orig", "gengamma",
  "gengamma.orig", "gompertz", "llogis", "lnorm", "lognormal",
  "weibull", "weibullPH".

  When the engine parameter is set to "flexsurvcure", options are "exp",
  "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull".

  When the engine parameter is set to "flexsurvspline", dists are
  ignored in favor of k and scale parameters.

  When the engine parameter is set to "survival", options are
  "exponential", "extreme", "gaussian", "loggaussian" (same as
  lognormal), "logistic", "lognormal", "rayleigh", "weibull".

  Default is
  `c("exp", "gamma", "gengamma", "gompertz", "llogis", "lnorm", "weibull")`
  which applies to flexsurv-related engines.

- engine:

  (Optional) The survival analysis engine to be used. Options are
  "flexsurv", "flexsurvcure", "flexsurvspline", and "survival". Default
  is "flexsurv".

  - "flexsurv" uses
    [`flexsurv::flexsurvreg()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvreg.md).

  - "flexsurvspline" uses
    [`flexsurv::flexsurvspline()`](http://chjackson.github.io/flexsurv-dev/reference/flexsurvspline.md).

  - "flexsurvcure" uses
    [`flexsurvcure::flexsurvcure()`](https://rdrr.io/pkg/flexsurvcure/man/flexsurvcure.html).

  - "survival" uses
    [`survival::survreg()`](https://rdrr.io/pkg/survival/man/survreg.html).

- k:

  (Optional) A numeric vector specifying the number of knots for
  spline-based models. Default is `c(1, 2, 3)` to test different
  numbers.

- scale:

  (Optional) A character vector specifying the scale parameter(s) for
  spline-based models. Options are "hazard", "odds", and "normal".
  Default is `"hazard"`.

- add_time_0:

  Optional. Uses
  [`survival::survfit0()`](https://rdrr.io/pkg/survival/man/survfit0.html)
  to add a starting time of 0 to the KM survfit object. This may be
  useful for plotting the KM at a subsequent stage (in surv_plots).
  Default is TRUE.

- ...:

  Additional arguments just to catch them and avoid errors.

## Value

A list containing information about the fit_models() call, the
distributions attempted, goodness of fit, fit averages, and cure
fractions (if applicable).

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
