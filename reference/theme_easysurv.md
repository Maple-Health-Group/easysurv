# Plot Theme for easysurv Survival and Hazard Plots

Plot Theme for easysurv Survival and Hazard Plots

## Usage

``` r
theme_easysurv()
```

## Value

A ggplot2 theme object.

## Examples

``` r
library(ggsurvfit)
fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
fit |> ggsurvfit() + theme_easysurv()
```
