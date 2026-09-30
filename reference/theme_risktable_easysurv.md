# Plot Theme for easysurv Risk Tables

To be used with
[`ggsurvfit::add_risktable()`](http://www.danieldsjoberg.com/ggsurvfit/reference/add_risktable.md).

## Usage

``` r
theme_risktable_easysurv()
```

## Value

A list containing a ggplot2 theme object.

## Examples

``` r
library(ggsurvfit)
fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
fit <- fit |> ggsurvfit() +
  theme_easysurv() +
  add_risktable(theme = theme_risktable_easysurv())
fit
```
