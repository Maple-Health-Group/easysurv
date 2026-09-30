# Export easysurv output to Excel via `openxlsx`

Export easysurv output to Excel via `openxlsx`

## Usage

``` r
write_to_xl(wb, object)
```

## Arguments

- wb:

  A Workbook object containing a worksheet

- object:

  The output of an easysurv command

## Value

An Excel workbook with the easysurv output.

## Examples

``` r
km_results <- get_km(
  data = easysurv::easy_bc,
  time = "recyrs",
  event = "censrec",
  group = "group",
  risktable_symbols = FALSE
)

wb <- openxlsx::createWorkbook()

if (FALSE) { # \dontrun{
write_to_xl(wb, km_results)
openxlsx::saveWorkbook(wb, "km_results.xlsx", overwrite = TRUE)
openxlsx::openXL("km_results.xlsx")
} # }
```
