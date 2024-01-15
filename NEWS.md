# easysurv 1.1.0

* `plot_fits()` now uses "flexsurv" to generate survival predictions, which matches the prediction method in `predict_fits()`. The original prediction method can be used instead by setting the new argument `plot_predictions = "survHE"` for `plot_fits()` and any functions that use `plot_fits()` (e.g., `quick_fit_select_()`, `quick_fit()`).
* `predict_fits()` now outputs a list object that includes 95% confidence intervals for the predicted survival probabilities. CIs can be excluded by setting the new argument `ci = FALSE` and the output will be a data.frame with just the survival predictions.

# easysurv 1.0.0

* Initial GitHub release.
