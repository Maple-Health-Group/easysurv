# Launch Example Survival Analysis Script using the easy_bc Data Set

This function launches an example script for starting survival analysis
using the easysurv package. The script uses a modified version of the bc
data set exported from the flexsurv package. The code is inspired by
[`usethis::use_template()`](https://usethis.r-lib.org/reference/use_template.html)
but modified to work outside the context of an .RProj or package.

## Usage

``` r
quick_start2(output_file_name = NULL)
```

## Arguments

- output_file_name:

  Optional. A file name to use for the script. Defaults to
  "easysurv_start.R" within a helper function.

## Value

A new R script file with example code.

## Examples

``` r
quick_start2()
#> ℹ easysurv template: Attempting to write a new .R file to a temporary directory.
#> ℹ Leaving /tmp/RtmpjWoZxh/easysurv_start.R unchanged.
#> ☐ Edit /tmp/RtmpjWoZxh/easysurv_start.R.
#> ℹ Remember to save the file to a permanent location if you wish to keep it.
```
