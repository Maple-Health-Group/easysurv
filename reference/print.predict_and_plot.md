# Print methods for `predict_and_plot()`

Print methods for
[`predict_and_plot()`](https://maple-health-group.github.io/easysurv/reference/predict_and_plot.md)

## Usage

``` r
# S3 method for class 'predict_and_plot'
print(x, ...)
```

## Arguments

- x:

  An object of class `predict_and_plot`

- ...:

  Additional arguments

## Value

A print summary of the `predict_and_plot` object.

## Examples

``` r
models <- fit_models(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  predict_by = "group"
)

predict_and_plot(models)
#> ℹ Survival plots have been printed.
#> ℹ Hazard plots have been printed.





```
