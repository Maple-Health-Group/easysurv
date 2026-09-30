# Cumulative Log Log Plot

Generates a Cumulative Log Log survival curve plot using
[`ggsurvfit::ggsurvfit()`](http://www.danieldsjoberg.com/ggsurvfit/reference/ggsurvfit.md)
with customizable options.

## Usage

``` r
plot_cloglog(
  fit,
  median_line = FALSE,
  legend_position = "top",
  plot_theme = theme_easysurv()
)
```

## Arguments

- fit:

  A [survival::survfit](https://rdrr.io/pkg/survival/man/survfit.html)
  object representing the survival data.

- median_line:

  Logical value indicating whether to include a line representing the
  median survival time. Default is `FALSE`.

- legend_position:

  Position of the legend in the plot. Default is "top".

- plot_theme:

  ggplot2 theme for the plot. Default is
  [`theme_easysurv()`](https://maple-health-group.github.io/easysurv/reference/theme_easysurv.md).

## Value

A ggplot object representing the cumulative log log plot.

## Examples

``` r
library(ggsurvfit)
fit <- survfit2(Surv(time, status) ~ surg, data = df_colon)
plot_cloglog(fit)
```
