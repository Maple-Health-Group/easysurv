# Package index

## Quick Start

These functions launch template scripts to help you get started.

- [`quick_start()`](https://maple-health-group.github.io/easysurv/reference/quick_start.md)
  : Launch Example Survival Analysis Script using the easy_lung Data Set
- [`quick_start2()`](https://maple-health-group.github.io/easysurv/reference/quick_start2.md)
  : Launch Example Survival Analysis Script using the easy_bc Data Set
- [`quick_start3()`](https://maple-health-group.github.io/easysurv/reference/quick_start3.md)
  : Launch Example Survival Analysis Script using the easy_adtte Data
  Set

## Example data sets

These example data sets are provided to help you explore package
functionality.

- [`easy_adtte`](https://maple-health-group.github.io/easysurv/reference/easy_adtte.md)
  :

  Formatted Copy of
  [ggsurvfit::adtte](http://www.danieldsjoberg.com/ggsurvfit/reference/adtte.md)

- [`easy_bc`](https://maple-health-group.github.io/easysurv/reference/easy_bc.md)
  :

  Formatted Copy of
  [flexsurv::bc](http://chjackson.github.io/flexsurv-dev/reference/bc.md)

- [`easy_lung`](https://maple-health-group.github.io/easysurv/reference/easy_lung.md)
  :

  Formatted Copy of
  [survival::lung](https://rdrr.io/pkg/survival/man/lung.html)

## Main functions

These functions are used to perform survival analysis.

- [`inspect_surv_data()`](https://maple-health-group.github.io/easysurv/reference/inspect_surv_data.md)
  : Inspect Survival Data

- [`get_km()`](https://maple-health-group.github.io/easysurv/reference/get_KM.md)
  : Generate Kaplan-Meier estimates

- [`test_ph()`](https://maple-health-group.github.io/easysurv/reference/test_PH.md)
  : Test Proportional Hazards Assumption

- [`fit_models()`](https://maple-health-group.github.io/easysurv/reference/fit_models.md)
  : Fit Survival Models

- [`predict_and_plot()`](https://maple-health-group.github.io/easysurv/reference/predict_and_plot.md)
  : Predict and Plot Fitted Models

- [`predict(`*`<fit_models>`*`)`](https://maple-health-group.github.io/easysurv/reference/predict.fit_models.md)
  :

  Predict method for `fit_models`

- [`plot(`*`<fit_models>`*`)`](https://maple-health-group.github.io/easysurv/reference/plot.fit_models.md)
  :

  Plot method for `fit_models`

## Print functions

These functions are used to print various stages of survival analysis.

- [`print(`*`<inspect_surv_data>`*`)`](https://maple-health-group.github.io/easysurv/reference/print.inspect_surv_data.md)
  :

  Print methods for
  [`inspect_surv_data()`](https://maple-health-group.github.io/easysurv/reference/inspect_surv_data.md)

- [`print(`*`<get_km>`*`)`](https://maple-health-group.github.io/easysurv/reference/print.get_km.md)
  :

  Print methods for
  [`get_km()`](https://maple-health-group.github.io/easysurv/reference/get_KM.md)

- [`print(`*`<test_ph>`*`)`](https://maple-health-group.github.io/easysurv/reference/print.test_ph.md)
  :

  Print methods for
  [`test_ph()`](https://maple-health-group.github.io/easysurv/reference/test_PH.md)

- [`print(`*`<fit_models>`*`)`](https://maple-health-group.github.io/easysurv/reference/print.fit_models.md)
  :

  Print methods for
  [`fit_models()`](https://maple-health-group.github.io/easysurv/reference/fit_models.md)

- [`print(`*`<predict_and_plot>`*`)`](https://maple-health-group.github.io/easysurv/reference/print.predict_and_plot.md)
  :

  Print methods for
  [`predict_and_plot()`](https://maple-health-group.github.io/easysurv/reference/predict_and_plot.md)

## Individual plot functions

These functions are used to plot various stages of survival analysis.

- [`plot_cloglog()`](https://maple-health-group.github.io/easysurv/reference/plot_cloglog.md)
  : Cumulative Log Log Plot
- [`plot_km()`](https://maple-health-group.github.io/easysurv/reference/plot_KM.md)
  : Plot Kaplan-Meier Data
- [`plot_schoenfeld()`](https://maple-health-group.github.io/easysurv/reference/plot_schoenfeld.md)
  : Plot Schoenfeld Residuals

## Themes

These functions are used to apply themes to plots.

- [`theme_easysurv()`](https://maple-health-group.github.io/easysurv/reference/theme_easysurv.md)
  : Plot Theme for easysurv Survival and Hazard Plots
- [`theme_risktable_easysurv()`](https://maple-health-group.github.io/easysurv/reference/theme_risktable_easysurv.md)
  : Plot Theme for easysurv Risk Tables

## Other functions

These functions are used to perform other tasks.

- [`get_schoenfeld()`](https://maple-health-group.github.io/easysurv/reference/get_schoenfeld.md)
  : Extract Schoenfeld Residuals

- [`write_to_xl()`](https://maple-health-group.github.io/easysurv/reference/write_to_xl.md)
  :

  Export easysurv output to Excel via `openxlsx`
