# Inspect Survival Data

Quickly inspect the survival data to ensure it is in the correct format.

## Usage

``` r
inspect_surv_data(data, time, event, group = NULL)
```

## Arguments

- data:

  A data frame containing the survival data.

- time:

  The column name in `data` containing the time-to-event data.

- event:

  The column name in `data` containing the event indicator data.

- group:

  Optional. The column name in `data` containing the group indicator
  data.

## Value

A list containing tibbles that summarise the first few rows of the
survival data, the sample sizes, the events, and median survival.

## Examples

``` r
inspect_surv_data(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  group = "group"
)
#> 
#> ── Inspect Survival Data ───────────────────────────────────────────────────────
#> 
#> ── First Few Rows ──
#> 
#> # A tibble: 6 × 4
#>   censrec rectime group recyrs
#>     <int>   <int> <fct>  <dbl>
#> 1       0    1342 Good    3.68
#> 2       0    1578 Good    4.32
#> 3       0    1760 Good    4.82
#> 4       0    1152 Good    3.16
#> 5       0     967 Good    2.65
#> 6       0     629 Good    1.72
#> 
#> ── Sample Sizes ──
#> 
#> # A tibble: 3 × 2
#>   group      n
#>   <fct>  <int>
#> 1 Good     229
#> 2 Medium   229
#> 3 Poor     228
#> 
#> ── Events Summary ──
#> 
#> # A tibble: 6 × 4
#>   group  censrec     n percent
#>   <fct>    <int> <int>   <dbl>
#> 1 Good         0   178   0.777
#> 2 Good         1    51   0.223
#> 3 Medium       0   126   0.550
#> 4 Medium       1   103   0.450
#> 5 Poor         0    83   0.364
#> 6 Poor         1   145   0.636
#> 
#> ── Survival Summary ──
#> 
#> # A tibble: 3 × 9
#>   records n.max n.start events rmean `se(rmean)` median `0.95LCL` `0.95UCL`
#>     <dbl> <dbl>   <dbl>  <dbl> <dbl>       <dbl>  <dbl>     <dbl>     <dbl>
#> 1     229   229     229     51  5.93       0.162  NA        NA        NA   
#> 2     229   229     229    103  4.60       0.186   5.25      4.12      5.57
#> 3     228   228     228    145  3.10       0.177   2.18      1.98      2.62
```
